-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.Polynomial.Rules where

import Common.Apply
import Common.Context
import Common.Rewriting
import Common.Transformation
import Common.Traversable
import Common.Uniplate (universe, uniplate)
import Common.Utils
import Common.View hiding (simplify)
import Control.Monad
import Data.List (nub, (\\), sort, sortBy)
import Data.Maybe
import Data.Ratio
import Domain.Math.Approximation (precision)
import Domain.Math.Clipboard
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Equation.CoverUpRules hiding (coverUpPlus)
import Domain.Math.Equation.BalanceRules
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.CleanUp
import Domain.Math.Polynomial.Views
import Domain.Math.Power.Views
import Domain.Math.Simplification
import Prelude hiding (repeat, (^), replicate)
import qualified Domain.Math.Data.Polynomial as P
import qualified Domain.Math.SquareRoot.Views as SQ

quadraticRuleOrder :: [String]
quadraticRuleOrder = 
   [ name coverUpTimes, name (coverUpMinusRightWith oneVar)
   , name (coverUpMinusLeftWith oneVar), name (coverUpPlusWith oneVar)
   , name coverUpPower
   , name commonFactorVar, name simplerPolynomial
   , name niceFactors, name noLinFormula
   , name cancelTerms, name sameConFactor, name distributionSquare
   ]

------------------------------------------------------------
-- General form rules: ax^2 + bx + c = 0

-- ax^2 + bx = 0 
commonFactorVar :: Rule (Equation Expr) 
commonFactorVar = rhsIsZero commonFactorVarNew

commonFactorVarNew :: Rule Expr
commonFactorVarNew = makeSimpleRule "common factor var" $ \expr -> do
   (x, (a, b, c)) <- match (polyNormalForm rationalView >>> second quadraticPolyView) expr
   guard (b /= 0 && c == 0)
   -- also search for constant factor
   let d | a<0 && b<0 = -gcdFrac a b
         | otherwise  = gcdFrac a b
   return (fromRational d .*. Var x .*. (fromRational (a/d) .*. Var x .+. fromRational (b/d)))

isInt :: MonadPlus m => Rational -> m Integer
isInt r = do
   guard (denominator r == 1)
   return (numerator r)

gcdFrac :: Rational -> Rational -> Rational
gcdFrac r1 r2 = fromMaybe 1 $ do 
   a <- isInt r1
   b <- isInt r2
   return (fromInteger (gcd a b))

-- ax^2 + c = 0
noLinFormula :: Rule (Equation Expr)
noLinFormula = liftRule myView $ 
   makeSimpleRule "no linear term b" $ \((x, (a, b, c)), rhs) -> do
      guard (rhs == 0 && b == 0 && c /= 0)
      return $ if a>0 then ((x, (a, 0, 0)), -c)
                      else ((x, (-a, 0, 0)), c)
 where
   myView = constantRight (polyNormalForm rationalView >>> second quadraticPolyView)

-- search for (X+A)*(X+B) decomposition 
niceFactors :: Rule (Equation Expr)
niceFactors = rhsIsZero niceFactorsNew

-- search for (X+A)*(X+B) decomposition 
niceFactorsNew :: Rule Expr
niceFactorsNew = makeSimpleRuleList "nice factors" $ \expr -> do
   let sign t@(x, (a, b, c)) = if a== -1 then (x, (1, -b, -c)) else t 
   (x, (a, b, c)) <- liftM sign (matchM (polyNormalForm integerView >>> second quadraticPolyView) expr)
   guard (a==1)
   let ok (i, j) = i+j == b
       f  (i, j) 
          | i == j = -- special case
              (Var x + fromInteger i) ^ 2
          | otherwise =
              (Var x + fromInteger i) * (Var x + fromInteger j)
   map f (filter ok (factors c))
 where
   factors :: Integer -> [(Integer, Integer)]
   factors n = [ pair
               | let h = floor (sqrt (abs (fromIntegral n)))
               , a <- [1..h], let b = n `div` a, a*b == n 
               , pair <- [(a, b), (negate a, negate b)] 
               ]

-- Simplify polynomial by multiplying (or dividing) the terms:
-- 1) If a,b,c are ints, then find gcd
-- 2) If any of a,b,c is a fraction, find lcm of denominators
-- 3) If a<0, then also suggest to change sign (return two solutions)
simplerPolynomial :: Rule (Equation Expr)
simplerPolynomial = rhsIsZero $ liftRuleIn thisView $ 
   makeSimpleRuleList "simpler polynomial" $ \(a, b, c) -> do
      r <- findFactor (filter (/=0) [a, b, c])
      d <- if a >= 0 then [r] else [-r, r]
      guard (d `notElem` [0, 1])
      return (a*d, b*d, c*d)
 where
   thisView = polyNormalForm rationalView >>> swapView >>> first quadraticPolyView
 
-- Simplified variant of simplerPoly: just bring a to 1.
-- Needed for quadratic strategy without square formula
bringAToOne :: Rule (Equation Expr)
bringAToOne = rhsIsZero $ liftRuleIn thisView $ 
   makeSimpleRule "bring a to one" $ \(a, b, c) -> do
   guard (a `notElem` [0, 1])
   return (1, b/a, c/a)
 where
   thisView = polyNormalForm rationalView >>> swapView >>> first quadraticPolyView

------------------------------------------------------------
-- General form rules: expr = 0

-- Rule must be symmetric in side of equation
mulZero :: Rule (OrList (Equation Expr))
mulZero = makeSimpleRuleList "multiplication is zero" $ onceJoinM bothSides
 where
   bothSides eq = oneSide eq `mplus` oneSide (flipSides eq)
   oneSide (lhs :==: rhs) = do
      guard (rhs == 0)
      (_, xs) <- matchM productView lhs
      guard (length xs > 1)
      return $ orList $ flip map xs $ \e ->
         case match (polyNormalForm rationalView >>> second linearPolyView) e of
            -- special cases (simplify immediately, as in G&R)
            Just (x, (a, b)) 
               | a == 1 -> 
                    Var x :==: fromRational (-b)
               | a == -1 -> 
                    Var x :==: fromRational b
            _ -> e :==: 0 

------------------------------------------------------------
-- Constant form rules: expr = constant

-- Use this configuration for covering-up plus and minus symbols!
-- Prevent    (x^2+3x)+5 = 0   to be covered up
oneVar :: ConfigCoverUp
oneVar = configCoverUp
   { configName        = Just "one var"
   , predicateCovered  = \a -> p1 a || p2 a
   , predicateCombined = noVars
   , coverLHS          = True
   , coverRHS          = True
   }
 where 
   p1 = (==1) . length . collectVars
   -- predicate p2 tests for cases such as 12*(x^2-3*x)+8 == 56
   p2 a = fromMaybe False $ do
      (x, y) <- match timesView a
      return (hasVars x /= hasVars y)

------------------------------------------------------------
-- Top form rules: expr1 = expr2

-- Do not simplify (5+sqrt 53)/2
simplerSquareRoot :: Rule Expr
simplerSquareRoot = makeSimpleRule "simpler square root" $ \e -> do
   xs <- f e
   guard (not (null xs))
   new <- canonical (SQ.squareRootViewWith rationalView) e
   ys <- f new
   guard (xs /= ys)
   return new
 where
   -- return numbers under sqrt symbol
   f :: Expr -> Maybe [Rational]
   f e = liftM sort $ sequence [ match rationalView e | Sqrt e <- universe e ]

cancelTerms :: Rule (Equation Expr)
cancelTerms = makeSimpleRule "cancel terms" $ \(lhs :==: rhs) -> do
   xs <- match sumView lhs
   ys <- match sumView rhs
   let zs = filter (`elem` ys) (nub xs)
   guard (not (null zs))
   let without as = build sumView (as \\ zs)
   return (without xs :==: without ys)

-- Two out of three "merkwaardige producten"
distributionSquare :: Rule Expr
distributionSquare = ruleList "distribution square"
   [ \a b -> (a+b)^2 :~> a^2 + 2*a*b + b^2
   , \a b -> (a-b)^2 :~> a^2 - 2*a*b + b^2
   ]

-- a^2 == b^2
squareBothSides :: Rule (OrList (Equation Expr))
squareBothSides = rule "square both sides" $ \a b -> 
   orList [a^2 :==: b^2] :~> orList [a :==: b, a :==: -b]

-- prepare splitting a square; turn lhs into x^2+bx+c such that (b/2)^2 is c
prepareSplitSquare :: Rule (Equation Expr)
prepareSplitSquare = liftRule myView $
   makeSimpleRule "prepare split square" $ \((x, (a, b, c)), r) -> do
      let newC   = (b/2)*(b/2)
          newRHS = r + newC - c
      guard (a==1 && b/=0 && c /= newC)
      return ((x, (a, b, newC)), newRHS)
 where
   myView = constantRight (polyNormalForm rationalView >>> second quadraticPolyView)

-- factor left-hand side into (ax + c)^2
factorLeftAsSquare :: Rule (Equation Expr)
factorLeftAsSquare = makeSimpleRule "factor left as square" $ \(lhs :==: rhs) -> do
   guard (noVars rhs)
   (x, (a, b, c)) <- match (polyNormalForm rationalView >>> second quadraticPolyView) lhs
   let h = b/2
   guard (a==1 && b/=0 && h*h == c)
   return ((Var x + build rationalView h)^2 :==: rhs) 

-- flip the two sides of an equation
flipEquation :: Rule (Equation Expr)
flipEquation = doBeforeTrans condition $
   rule "flip equation" $ \a b ->
      (a :==: b) :~> (b :==: a)
 where
   condition = makeTrans $ \eq@(lhs :==: rhs) -> do
      guard (hasVars rhs && noVars lhs)
      return eq

-- Afterwards, merge and sort
moveToLeft :: Rule (Equation Expr)
moveToLeft = makeSimpleRule "move to left" $ \(lhs :==: rhs) -> do
   guard (rhs /= 0 && hasVars lhs && (hasVars rhs || isComplex lhs))
   return (collectLikeTerms (sorted (lhs - rhs)) :==: 0)
 where
   isComplex = maybe False ((>= 2) . length . filter hasVars) 
             . match sumView . applyD merge
 
   -- high exponents first, non power-factor terms at the end
   sorted = simplifyWith (sortBy f) sumView
   f a b  = toPF a `compare` toPF b
   toPF   = fmap (negate . thd3) . match powerFactorView

ruleApproximate :: Rule (Relation Expr)
ruleApproximate = makeSimpleRule "approximate" $ \relation -> do
   lhs :==: rhs <- match equationView relation
   guard (not (simplify rhs `belongsTo` rationalView))
   x <- getVariable lhs
   d <- match doubleView rhs
   let new = fromDouble (precision 4 d)
   return (Var x .~=. new)

ruleNormalizeRational :: Rule Expr
ruleNormalizeRational = 
   ruleFromView "normalize rational number" rationalView

ruleNormalizeMixedFraction :: Rule Expr
ruleNormalizeMixedFraction = 
   ruleFromView "normalize mixed fraction" mixedFractionView

-----------------------------------------------------------
-------- Rules From HDE

-- X*A + X*B = X*C + X*D
allPowerFactors :: Rule (OrList (Equation Expr))
allPowerFactors = makeSimpleRule "all power factors" $ onceJoinM $ \(lhs :==: rhs) -> do
   xs <- match (sumView >>> listView powerFactorView) lhs
   ys <- match (sumView >>> listView powerFactorView) rhs
   case unzip3 (filter ((/=0) . snd3) (xs ++ ys)) of
      (s:ss, _, ns) | all (==s) ss -> do
         let m = minimum ns 
             make = build (sumView >>> listView powerFactorView) . map f
             f (s, i, n) = (s, i, n-m)
         guard (m > 0 && length ns > 1)
         return $ orList [Var s :==: 0, make xs :==: make ys]
      _ -> Nothing

-- A*B = A*C  implies  A=0 or B=C
sameFactor :: Rule (OrList (Equation Expr))
sameFactor = makeSimpleRule "same factor" $ onceJoinM $ \(lhs :==: rhs) -> do
   (b1, xs) <- match productView lhs
   (b2, ys) <- match productView rhs
   (x, y) <- safeHead [ (x, y) | x <- xs, y <- ys, x==y, hasVars x ] -- equality is too strong?
   return $ orList [ x :==: 0, build productView (b1, xs\\[x]) :==: build productView (b2, ys\\[y]) ]

-- N*(A+B) = N*C + N*D   recognize a constant factor on both sides
-- Example: 3(x^2+1/2) = 6+6x
sameConFactor :: Rule (Equation Expr)
sameConFactor = liftRule myView $ 
   makeSimpleRule "same constant factor" $ \(ps1 :==: ps2) -> do
      let (bs, zs) = unzip (ps1 ++ ps2)
          (rs, es) = unzip (map (f 1 []) zs)
          f r acc []     = (r, reverse acc)
          f r acc (x:xs) = case match rationalView x of
                              Just r2 -> f (r*r2) acc xs
                              Nothing -> f r (x:acc) xs
      con <- whichCon rs
      guard (con /= 1)
      let make b r e          = (b, fromRational (r/con):e)
          (newLeft, newRight) = splitAt (length ps1) (zipWith3 make bs rs es)
      return (newLeft :==: newRight)
 where
   myView = bothSidesView (sumView >>> listView productView)
 
   whichCon :: [Rational] -> Maybe Rational
   whichCon xs 
      | all (\x -> denominator x == 1 && x /= 0) xs =
           Just (fromInteger (foldr1 gcd (map numerator xs)))
      | otherwise = Nothing

abcFormula :: Rule (Context (OrList (Equation Expr)))
abcFormula = makeSimpleRule "abc formula" $ withCM $ onceJoinM $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (x, (a, b, c)) <- matchM (polyNormalForm rationalView >>> second quadraticPolyView) lhs
   addListToClipboard ["a", "b", "c"] (map fromRational [a, b, c])
   let discr = b*b - 4 * a * c
       sqD   = sqrt (fromRational discr)
   addToClipboard "D" (fromRational discr)
   case compare discr 0 of
      LT -> return false
      EQ -> return $ return $ 
         Var x :==: (-fromRational b) / (2 * fromRational a)
      GT -> return $ orList
         [ Var x :==: (-fromRational b + sqD) / (2 * fromRational a)
         , Var x :==: (-fromRational b - sqD) / (2 * fromRational a)
         ]

higherSubst :: Rule (Context (Equation Expr))
higherSubst = makeSimpleRule "higher subst" $ withCM $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   let myView = polyView >>> second trinomialPolyView
   (x, ((a, n1), (b, n2), (c, n3))) <- matchM myView lhs
   guard (n1 == 0 && n2 > 1 && n3 `mod` n2 == 0 && x /= "p")
   let new = build myView ("p", ((a, 0), (b, 1), (c, n3 `div` n2)))
   addToClipboard "subst" (toExpr (Var "p" :==: Var x .^. fromIntegral n2))
   return (new :==: 0)

substBackVar :: Rule (Context Expr)
substBackVar = makeSimpleRule "subst back var" $ withCM $ \a -> do
   expr <- lookupClipboard "subst"
   case fromExpr expr of
      Just (Var p :==: rhs) -> do
         guard (p `elem` collectVars a)
         return (subst p rhs a)
      _ -> fail "no subst in clipboard"
 where
   subst a b (Var c) | a==c = b
   subst a b expr = build (map (subst a b) cs)
    where (cs, build) = uniplate expr

exposeSameFactor :: Rule (Equation Expr)
exposeSameFactor = liftRule (bothSidesView productView) $ 
   makeSimpleRuleList "expose same factor" $ \((bx, xs) :==: (by, ys)) -> do 
      (nx, ny) <- [ (xs, new) | x <- xs, suitable x, new <- exposeList x ys ] ++
                  [ (new, ys) | y <- ys, suitable y, new <- exposeList y xs ]
      return ((bx, nx) :==: (by, ny))
 where
   suitable p = fromMaybe False $ do 
      (_, _, b) <- match (linearViewWith rationalView) p
      guard (b /= 0)
      return True
   
   exposeList _ [] = []
   exposeList a (b:bs) = map (++bs) (expose a b) ++ map (b:) (exposeList a bs)
   
   expose a b = do
      (s1, p1) <- matchM (polyViewWith rationalView) a
      (s2, p2) <- matchM (polyViewWith rationalView) b
      guard (s1==s2 && p1/=p2)
      case P.division p2 p1 of
         Just p3 -> return $ map (\p -> build (polyViewWith rationalView) (s1,p)) [p1, p3]
         Nothing -> []

---------------------------------------------------------
-- From LinearEquations

-- Only used for cleaning up
distributeAll :: Expr -> Expr
distributeAll expr = 
   case expr of 
      a :*: b -> let as = fromMaybe [a] (match sumView a)
                     bs = fromMaybe [b] (match sumView b)
                 in build sumView [ a .*. b | a <- as, b <- bs ]
      _ -> expr

-- This rule should consider the associativity of multiplication
-- Combine bottom-up, for example:  5*(x-5)*(x+5) 
-- However, in  -2x(2x+10)   (-2x) should be seen as "one term"
distributionT :: Transformation Expr
distributionT = makeTransList f
 where
   f expr = do
      (b, xs) <- matchM simpleProductView expr
      ys      <- rec (combine xs)
      return $ build simpleProductView (b, ys)
   
   combine :: [Expr] -> [Expr]
   combine (x:y:rest) | p x && p y = combine ((x*y):rest)
    where p = maybe False ((==1) . length) . match sumView
   combine []     = []
   combine (x:xs) = x : combine xs
   
   rec :: [Expr] -> [[Expr]]
   rec (a:b:xs) = map (:xs) (g a b) ++ map (a:) (rec (b:xs))
   rec _        = []
   
   g :: Expr -> Expr -> [Expr]
   g a b = do 
      as <- matchM sumView a
      bs <- matchM sumView b
      guard (length as > 1 || length bs > 1)
      return $ build sumView [ a .*. b | a <- as, b <- bs ]

-------------------------------------------------------
-- Rewrite Rules

varToLeft :: Rule (Relation Expr)
varToLeft = doAfter (fmap collectLikeTerms) $ 
   makeRule "variable to left" $ flip supply1 minusT $ \eq -> do
      (x, a, _) <- match (linearViewWith rationalView) (rightHandSide eq)
      guard (a/=0)
      return (fromRational a * Var x)

-- factor is always positive due to lcm function
removeDivision :: Rule (Relation Expr)
removeDivision = doAfter (fmap (collectLikeTerms . distributeAll)) $
   makeRule "remove division" $ flip supply1 timesT $ \eq -> do
      xs <- match sumView (leftHandSide eq)
      ys <- match sumView (rightHandSide eq)
      -- also consider parts without variables
      -- (but at least one participant should have a variable)
      zs <- forM (xs ++ ys) $ \a -> do
               (_, list) <- match productView a
               return [ (hasVars a, e) | e <- list ]
      let f (b, e) = do 
             (_, this) <- match (divView >>> second integerView) e
             return (b, this)
      case mapMaybe f (concat zs) of
         [] -> Nothing
         ps -> let (bs, ns) = unzip ps
               in if or bs then return (fromInteger (foldr1 lcm ns))
                           else Nothing

distributeTimes :: Rule Expr
distributeTimes = makeSimpleRuleList "distribution multiplication" $
   liftM collectLikeTerms . applyAll distributionT

distributeDivision :: Rule Expr
distributeDivision = makeSimpleRule "distribution division" $ \expr -> do
   (xs, r) <- match (divView >>> (sumView *** rationalView)) expr
   guard (length xs > 1)
   let ys = map (/fromRational r) xs
   return $ build sumView ys

merge :: Rule Expr
merge = makeSimpleRule "merge similar terms" $ \old -> do
   let new = collectLikeTerms old
   guard (old /= new)
   return new
   
ruleFromView :: Eq a => String -> View a b -> Rule a
ruleFromView s v = makeSimpleRuleList s $ \a -> do
   b <- canonicalM v a
   guard (a /= b)
   return b
   
rhsIsZero :: Rule Expr -> Rule (Equation Expr)
rhsIsZero r = makeSimpleRuleList (name r) $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   a <- applyAll r lhs
   return (a :==: rhs)
   
constantRight :: View Expr a -> View (Equation Expr) (a, Rational)
constantRight v = makeView f g
 where
   f (lhs :==: rhs) = liftM2 (,) (match v lhs) (match rationalView rhs)
   g (a, r) = build v a :==: build rationalView r

bothSidesView :: View a b -> View (Equation a) (Equation b)
bothSidesView v = makeView f (fmap (build v))
 where
   f (lhs :==: rhs) = liftM2 (:==:) (match v lhs) (match v rhs)

findFactor :: Monad m => [Rational] -> m Rational
findFactor rs
   | null rs = 
        fail "no factor"
   | all ((==1) . denominator) rs = 
        return $ Prelude.recip $ fromIntegral $ foldr1 gcd $ map numerator rs
   | otherwise = 
        return $ fromIntegral $ foldr1 lcm $ map denominator rs