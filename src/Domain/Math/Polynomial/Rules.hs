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
module Domain.Math.Polynomial.Rules 
   ( sameConFactor, abcFormula, allPowerFactors, bringAToOne, cancelTerms
   , commonFactorVar, commonFactorVarNew, defPowerNat, distributeDivision
   , distributeTimes, distributionSquare, exposeSameFactor, factorLeftAsSquare
   , factorVariablePower, flipEquation, higherSubst, merge, moveToLeft, mulZero
   , niceFactors, niceFactorsNew, noDivisionConstant, noLinFormula, oneVar
   , parentNotNegCheck, prepareSplitSquare, quadraticRuleOrder, removeDivision
   , ruleApproximate, ruleNormalizeMixedFraction, ruleNormalizeRational
   , sameFactor, simplerLinearFactor, simplerPolynomial, simplerSquareRoot
   , squareBothSides, substBackVar, varToLeft, conditionVarsRHS
   ) where

import Common.Library hiding (terms, simplify)
import Common.Uniplate (universe, descend)
import Common.Utils
import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord
import Data.Ratio
import Domain.Math.Approximation (precision)
import Domain.Math.Data.OrList
import Domain.Math.Data.Polynomial
import Domain.Math.Data.Relation
import Domain.Math.Equation.BalanceRules
import Domain.Math.Equation.CoverUpRules
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.Views
import Domain.Math.Power.OldViews (powerFactorView)
import Domain.Math.Simplification hiding (simplifyWith)
import Domain.Math.SquareRoot.Views 
import Prelude hiding ( (^) )

quadraticRuleOrder :: [Id]
quadraticRuleOrder = 
   [ getId coverUpTimes, getId (coverUpMinusRightWith oneVar)
   , getId (coverUpMinusLeftWith oneVar), getId (coverUpPlusWith oneVar)
   , getId coverUpPower
   , getId commonFactorVar, getId simplerPolynomial
   , getId niceFactors, getId noLinFormula
   , getId cancelTerms, getId sameConFactor, getId distributionSquare
   , getId allPowerFactors
   ]

lineq, quadreq, polyeq :: String
lineq   = "algebra.equations.linear"
quadreq = "algebra.equations.quadratic"
polyeq  = "algebra.equations.polynomial"

------------------------------------------------------------
-- General form rules: ax^2 + bx + c = 0

quadraticNF :: View Expr (String, (Rational, Rational, Rational))
quadraticNF = polyNormalForm rationalView >>> second quadraticPolyView

-- ax^2 + bx = 0 
commonFactorVar :: Rule (Equation Expr) 
commonFactorVar = rhsIsZero commonFactorVarNew

-- Maybe to be replaced by more general factorVariablePower??
commonFactorVarNew :: Rule Expr
commonFactorVarNew = describe "Common factor variable" $ 
   makeSimpleRule (quadreq, "common-factor") $ \expr -> do
      (x, (a, b, c)) <- match quadraticNF expr
      guard (b /= 0 && c == 0)
      -- also search for constant factor
      let d | a<0 && b<0 = -gcdFrac a b
            | otherwise  = gcdFrac a b
      return (fromRational d .*. Var x .*. (fromRational (a/d) .*. Var x .+. fromRational (b/d)))

gcdFrac :: Rational -> Rational -> Rational
gcdFrac r1 r2 = 
   if denominator r1 == 1 && denominator r2 == 1
   then fromInteger (numerator r1 `gcd` numerator r2)
   else 1

-- ax^2 + c = 0
noLinFormula :: Rule (Equation Expr)
noLinFormula = describe "No linear term ('b=0')" $ liftRule myView $ 
   makeSimpleRule (quadreq, "no-lin") $ \((x, (a, b, c)), rhs) -> do
      guard (rhs == 0 && b == 0 && c /= 0)
      return $ if a>0 then ((x, (a, 0, 0)), -c)
                      else ((x, (-a, 0, 0)), c)
 where
   myView = constantRight quadraticNF

-- search for (X+A)*(X+B) decomposition 
niceFactors :: Rule (Equation Expr)
niceFactors = rhsIsZero niceFactorsNew

-- search for (X+A)*(X+B) decomposition 
niceFactorsNew :: Rule Expr
niceFactorsNew = describe "Find a nice decomposition" $ 
   makeSimpleRuleList (quadreq, "nice-factors") $ \expr -> do
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
               | let h = (floor :: Double -> Integer) (sqrt (abs (fromIntegral n)))
               , a <- [1..h], let b = n `div` a, a*b == n 
               , pair <- [(a, b), (negate a, negate b)] 
               ]

-- Simplify polynomial by multiplying (or dividing) the terms:
-- 1) If a,b,c are ints, then find gcd
-- 2) If any of a,b,c is a fraction, find lcm of denominators
-- 3) If a<0, then also suggest to change sign (return two solutions)
simplerPolynomial :: Rule (Equation Expr)
simplerPolynomial = describe "simpler polynomial" $
   rhsIsZero $ liftRuleIn (quadraticNF >>> swapView) $ 
   makeSimpleRuleList (quadreq, "simpler-poly") $ \(a, b, c) -> do
      r <- findFactor (filter (/=0) [a, b, c])
      d <- if a >= 0 then [r] else [-r, r]
      guard (d `notElem` [0, 1])
      return (a*d, b*d, c*d)
 
-- Simplified variant of simplerPoly: just bring a to 1.
-- Needed for quadratic strategy without square formula
bringAToOne :: Rule (Equation Expr)
bringAToOne = rhsIsZero $ liftRuleIn (quadraticNF >>> swapView) $ 
   describe "Bring 'a' to one" $ 
   makeSimpleRule (quadreq, "scale") $ \(a, b, c) -> do
   guard (a `notElem` [0, 1])
   return (1, b/a, c/a)

------------------------------------------------------------
-- General form rules: expr = 0

-- Rule must be symmetric in side of equation
mulZero :: Rule (OrList (Equation Expr))
mulZero = describe "multiplication is zero" $ 
   makeSimpleRuleList (quadreq, "product-zero") $ oneDisjunct bothSides
 where
   bothSides eq = oneSide eq `mplus` oneSide (flipSides eq)
   oneSide (lhs :==: rhs) = do
      guard (rhs == 0)
      (_, xs) <- matchM productView lhs
      guard (length xs > 1)
      return $ toOrList $ flip map xs $ \e ->
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
   { configName        = "onevar"
   , predicateCovered  = \a -> p1 a || p2 a
   , predicateCombined = hasNoVar
   , coverLHS          = True
   , coverRHS          = True
   }
 where 
   p1 = (==1) . length . vars
   -- predicate p2 tests for cases such as 12*(x^2-3*x)+8 == 56
   p2 a = fromMaybe False $ do
      (x, y) <- match timesView a
      return (hasSomeVar x /= hasSomeVar y)

------------------------------------------------------------
-- Top form rules: expr1 = expr2

-- Do not simplify (5+sqrt 53)/2
simplerSquareRoot :: Rule Expr
simplerSquareRoot = describe "simpler square root" $ 
   makeSimpleRule (quadreq, "simpler-sqrt") $ \e -> do
      xs <- f e
      guard (not (null xs))
      new <- canonical (squareRootViewWith rationalView) e
      ys <- f new
      guard (xs /= ys)
      return new
 where
   -- return numbers under sqrt symbol
   f :: Expr -> Maybe [Rational]
   f e = liftM sort $ sequence [ match rationalView a | Sqrt a <- universe e ]

cancelTerms :: Rule (Equation Expr)
cancelTerms = describe "Cancel terms" $ 
   makeSimpleRule (quadreq, "cancel") $ \(lhs :==: rhs) -> do
   xs <- match sumView lhs
   ys <- match sumView rhs
   let zs = filter (`elem` ys) (nub xs)
   guard (not (null zs))
   let without as = build sumView (as \\ zs)
   return (without xs :==: without ys)

-- Two out of three "merkwaardige producten"
distributionSquare :: Rule Expr
distributionSquare = describe "distribution square" $
   ruleList (quadreq, "distr-square")
      [ \a b -> (a+b)^2 :~> a^2 + 2*a*b + b^2
      , \a b -> (a-b)^2 :~> a^2 - 2*a*b + b^2
      ]

-- a^2 == b^2
squareBothSides :: Rule (OrList (Equation Expr))
squareBothSides = describe "square both sides" $ 
   rule (quadreq, "square-both") $ \a b -> 
   to (a^2 :==: b^2) :~> toOrList [a :==: b, a :==: -b]

-- prepare splitting a square; turn lhs into x^2+bx+c such that (b/2)^2 is c
prepareSplitSquare :: Rule (Equation Expr)
prepareSplitSquare = describe "prepare split square" $ 
   liftRule myView $
   makeSimpleRule (quadreq, "prepare-split") $ \((x, (a, b, c)), r) -> do
      let newC   = (b/2)*(b/2)
          newRHS = r + newC - c
      guard (a==1 && b/=0 && c /= newC)
      return ((x, (a, b, newC)), newRHS)
 where
   myView = constantRight quadraticNF

-- factor left-hand side into (ax + c)^2
factorLeftAsSquare :: Rule (Equation Expr)
factorLeftAsSquare = describe "factor left as square" $
   makeSimpleRule (quadreq, "left-square") $ \(lhs :==: rhs) -> do
      guard (hasNoVar rhs)
      (x, (a, b, c)) <- match quadraticNF lhs
      let h = b/2
      guard (a==1 && b/=0 && h*h == c)
      return ((Var x + build rationalView h)^2 :==: rhs) 

-- flip the two sides of an equation
flipEquation :: Rule (Equation Expr)
flipEquation = describe "flip equation" $
   rule (lineq, "flip") $ \a b ->
      (a :==: b) :~> (b :==: a)

conditionVarsRHS :: Rule (Equation Expr)
conditionVarsRHS = describe "All variables are in the right-hand side" $ 
   checkRule $ \(lhs :==: rhs) -> 
      hasSomeVar rhs && hasNoVar lhs

-- Afterwards, merge and sort
moveToLeft :: Rule (Equation Expr)
moveToLeft = describe "Move to left" $
   makeSimpleRule (quadreq, "move-left") $ \(lhs :==: rhs) -> do
      guard (rhs /= 0 && hasSomeVar lhs && (hasSomeVar rhs || isComplex lhs))
      return (collectLikeTerms (sorted (lhs - rhs)) :==: 0)
 where
   isComplex = maybe False ((>= 2) . length . filter hasSomeVar) 
             . match sumView . applyD merge
 
   -- high exponents first, non power-factor terms at the end
   sorted = simplifyWith (sortBy (comparing toPF)) sumView
   toPF   = fmap (negate . thd3) . match powerFactorView

ruleApproximate :: Rule (Relation Expr)
ruleApproximate = describe "Approximate irrational number" $
   makeSimpleRule (quadreq, "approx") $ \relation -> do
      lhs :==: rhs <- match equationView relation
      guard (not (simplify rhs `belongsTo` rationalView))
      x <- getVariable lhs
      d <- match doubleView rhs
      let new = fromDouble (precision 4 d)
      return (Var x .~=. new)

ruleNormalizeRational :: Rule Expr
ruleNormalizeRational =
   describe "normalize rational number" $ 
   ruleFromView (lineq, "norm-rational") rationalView

ruleNormalizeMixedFraction :: Rule Expr
ruleNormalizeMixedFraction = 
   describe "normalize mixed fraction" $
   ruleFromView (lineq, "norm-mixed") mixedFractionView

-----------------------------------------------------------
-------- Rules From HDE

-- X*A + X*B = X*C + X*D
-- New implementation, but slightly different than original
-- This one does not factor constants

allPowerFactors :: Rule (OrList (Equation Expr))
allPowerFactors = describe "all power factors" $
   makeSimpleRule (polyeq, "power-factors") $ oneDisjunct $ 
   \(lhs :==: rhs) -> do
      let myView = polyNormalForm rationalView
      (s1, p1) <- match myView lhs
      (s2, p2) <- match myView rhs
      let n | p1 == 0   = lowestDegree p2
            | p2 == 0   = lowestDegree p1 
            | otherwise = lowestDegree p1 `min` lowestDegree p2
          ts  = terms p1 ++ terms p2
          f p = build myView (s1, raise (-n) p)
      guard ((s1==s2 || p1==0 || p2==0) && n > 0 && length ts > 1)
      return $ toOrList [Var s1 :==: 0, f p1 :==: f p2] 

factorVariablePower :: Rule Expr
factorVariablePower = describe "factor variable power" $ 
   makeSimpleRule (polyeq, "factor-varpower") $ \expr -> do
   let myView = polyNormalForm rationalView
   (s, p) <- match (polyNormalForm rationalView) expr
   let n = lowestDegree p
   guard (n > 0 && length (terms p) > 1)
   return $ Var s .^. fromIntegral n * build myView (s, raise (-n) p)

-- A*B = A*C  implies  A=0 or B=C
sameFactor :: Rule (OrList (Equation Expr))
sameFactor = describe "same factor" $ 
   makeSimpleRule (quadreq, "same-factor") $ oneDisjunct $ \(lhs :==: rhs) -> do
      (b1, xs) <- match productView lhs
      (b2, ys) <- match productView rhs
      (x, y) <- safeHead [ (x, y) | x <- xs, y <- ys, x==y, hasSomeVar x ] -- equality is too strong?
      return $ toOrList [ x :==: 0, build productView (b1, xs\\[x]) :==: build productView (b2, ys\\[y]) ]

-- N*(A+B) = N*C + N*D   recognize a constant factor on both sides
-- Example: 3(x^2+1/2) = 6+6x
sameConFactor :: Rule (Equation Expr)
sameConFactor = 
   describe "same constant factor" $
   liftRule myView $ 
   makeSimpleRule (quadreq, "same-con-factor") $ \(ps1 :==: ps2) -> do
      let (bs, zs) = unzip (ps1 ++ ps2)
          (rs, es) = unzip (map (f 1 []) zs)
          f r acc []     = (r, reverse acc)
          f r acc (x:xs) = case match rationalView x of
                              Just r2 -> f (r*r2) acc xs
                              Nothing -> f r (x:acc) xs
      c <- whichCon rs
      guard (c /= 1)
      let make b r e          = (b, fromRational (r/c):e)
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
abcFormula = describe "quadratic formula (abc formule)" $ 
   makeSimpleRule (quadreq, "abc") $ withCM $ oneDisjunct $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (x, (a, b, c)) <- matchM quadraticNF lhs
   addListToClipboard ["a", "b", "c"] (map fromRational [a, b, c])
   let discr = b*b - 4 * a * c
       sqD   = sqrt (fromRational discr)
   addToClipboard "D" (fromRational discr)
   case compare discr 0 of
      LT -> return false
      EQ -> return $ to $ 
         Var x :==: (-fromRational b) / (2 * fromRational a)
      GT -> return $ toOrList
         [ Var x :==: (-fromRational b + sqD) / (2 * fromRational a)
         , Var x :==: (-fromRational b - sqD) / (2 * fromRational a)
         ]

higherSubst :: Rule (Context (Equation Expr))
higherSubst = describe "Substitute variable" $
   makeSimpleRule (polyeq, "subst") $ withCM $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   let myView = polyView >>> second trinomialPolyView
   (x, ((a, n1), (b, n2), (c, n3))) <- matchM myView lhs
   guard (n1 == 0 && n2 > 1 && n3 `mod` n2 == 0 && x /= "p")
   let new = build myView ("p", ((a, 0), (b, 1), (c, n3 `div` n2)))
   addToClipboard "subst" (toExpr (Var "p" :==: Var x .^. fromIntegral n2))
   return (new :==: 0)

substBackVar :: Rule (Context Expr)
substBackVar = describe "Substitute back a variable" $ 
   makeSimpleRule (polyeq, "back-subst") $ withCM $ \a -> do
   expr <- lookupClipboard "subst"
   case fromExpr expr of
      Just (Var p :==: rhs) -> do
         guard (hasVar p a)
         return (subst p rhs a)
      _ -> fail "no subst in clipboard"
 where
   subst a b (Var c) | a==c = b
   subst a b expr = descend (subst a b) expr

exposeSameFactor :: Rule (Equation Expr)
exposeSameFactor = describe "expose same factor" $ 
   liftRule (bothSidesView productView) $ 
   makeSimpleRuleList (polyeq, "expose-factor") $ \((bx, xs) :==: (by, ys)) -> do 
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
      case division p2 p1 of
         Just p3 -> return $ map (\p -> build (polyViewWith rationalView) (s1,p)) [p1, p3]
         Nothing -> []

---------------------------------------------------------
-- From LinearEquations

-- Only used for cleaning up
distributeAll :: Expr -> Expr
distributeAll expr = 
   case expr of 
      e1 :*: e2 -> let as = fromMaybe [e1] (match sumView e1)
                       bs = fromMaybe [e2] (match sumView e2)
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
   g e1 e2 = do 
      as <- matchM sumView e1 
      bs <- matchM sumView e2
      guard (length as > 1 || length bs > 1)
      return $ build sumView [ a .*. b | a <- as, b <- bs ]

-------------------------------------------------------
-- Rewrite Rules

varToLeft :: Rule (Relation Expr)
varToLeft = doAfter (fmap collectLikeTerms) $ 
   describe "variable to left" $ 
   makeRule (lineq, "var-left") $ flip supply1 minusT $ \eq -> do
      (x, a, _) <- match (linearViewWith rationalView) (rightHandSide eq)
      guard (a/=0)
      return (fromRational a * Var x)

-- factor is always positive due to lcm function
removeDivision :: Rule (Relation Expr)
removeDivision = doAfter (fmap (collectLikeTerms . distributeAll)) $
   describe "remove division" $ 
   makeRule (lineq, "remove-div") $ flip supply1 timesT $ \eq -> do
      xs <- match sumView (leftHandSide eq)
      ys <- match sumView (rightHandSide eq)
      -- also consider parts without variables
      -- (but at least one participant should have a variable)
      zs <- forM (xs ++ ys) $ \a -> do
               (_, list) <- match productView a
               return [ (hasSomeVar a, e) | e <- list ]
      let f (b, e) = do 
             (_, this) <- match (divView >>> second integerView) e
             return (b, this)
          (bs, ns) = unzip (mapMaybe f (concat zs))
      guard (or bs)
      return (fromInteger (foldr1 lcm ns))

distributeTimes :: Rule Expr
distributeTimes = describe "distribution multiplication" $ 
   makeSimpleRuleList (lineq, "distr-times") $
      liftM collectLikeTerms . applyAll distributionT

distributeDivision :: Rule Expr
distributeDivision = describe "distribution division" $
   makeSimpleRule (quadreq, "distr-div") $ \expr -> do
      (xs, r) <- match (divView >>> (sumView *** rationalView)) expr
      guard (length xs > 1)
      let ys = map (/fromRational r) xs
      return $ build sumView ys

merge :: Rule Expr
merge = describe "merge similar terms" $ 
   makeSimpleRule (lineq, "merge") $ \old -> do
      let new = collectLikeTerms old
          f = maybe 0 length . match sumView
      guard (f old > f new)
      return new

simplerLinearFactor :: Rule Expr
simplerLinearFactor = describe "simpler linear factor" $ 
   makeSimpleRule (polyeq, "simpler-linfactor") $ \expr -> do
   let myView = polyNormalForm rationalView >>> second linearPolyView
   (x, (a, b)) <- match myView expr
   let d = (if a<0 then negate else id) (gcdFrac a b)
   guard (a /= 0 && b /= 0 && d /= 1 && d /= -1)
   return $ fromRational d * build myView (x, (a/d, b/d))
   
ruleFromView :: (IsId n, Eq a) => n -> View a b -> Rule a
ruleFromView s v = makeSimpleRule s $ \a -> do
   b <- canonical v a
   guard (a /= b)
   return b
   
rhsIsZero :: Rule Expr -> Rule (Equation Expr)
rhsIsZero r = makeSimpleRuleList (showId r) $ \(lhs :==: rhs) -> do
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
        
parentNotNegCheck :: Rule (Context Expr)
parentNotNegCheck = minorRule $ makeSimpleRule "parent not negate check" $ \c -> 
   case up c >>= current of
      Just (Negate _) -> Nothing
      _               -> Just c
      
noDivisionConstant :: Rule Expr
noDivisionConstant = makeSimpleRule (lineq, "no-div-con") f
 where
   f (a :/: b) | hasNoVar b && hasSomeVar a = 
      return ((1/b) * a)
   f _ = Nothing
   
defPowerNat :: Rule Expr
defPowerNat = makeSimpleRule (polyeq, "def-power-nat") f
 where
   f (Sym _ [Var _, _]) = Nothing -- should not work on x^5
   f (Sym s [a, Nat n]) | isPowerSymbol s = 
      return (build productView (False, replicate (fromInteger n) a))
   f _ = Nothing