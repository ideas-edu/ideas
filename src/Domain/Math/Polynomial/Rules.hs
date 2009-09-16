module Domain.Math.Polynomial.Rules where

import Common.Apply
import Common.Context
import Common.Transformation
import Common.Traversable
import Common.Uniplate (somewhereM)
import Common.Utils
import Common.View
import Domain.Math.Numeric.Views
import Control.Monad
import Data.List (nub, (\\))
import Data.Maybe
import Data.Ratio
import Domain.Math.Data.Equation
import Domain.Math.Data.OrList
import Domain.Math.Equation.CoverUpRules hiding (coverUpPlus)
import Domain.Math.Expr
import Domain.Math.Polynomial.Views
import Domain.Math.Polynomial.CleanUp
import Domain.Math.Power.Views
import Prelude hiding (repeat, (^), replicate)
import qualified Domain.Math.Data.SquareRoot as SQ
import qualified Prelude


------------------------------------------------------------
-- Rule collection

linearRules :: [Rule (Context (Equation Expr))]
linearRules = map ignoreContext
   [ removeDivision, ruleMulti merge, ruleOnce distribute
   , varToLeft, conToRight, scaleToOne
   ]

quadraticRules :: [Rule (OrList (Equation Expr))]
quadraticRules = 
   [ ruleOnce noConFormula, ruleOnce noLinFormula, ruleOnce niceFactors
   , ruleOnce simplerA, abcFormula, mulZero, coverUpPower, ruleOnce coverUpPlus
   , ruleOnce coverUpTimes, ruleOnce coverUpNegate, ruleOnce coverUpNumerator
   , ruleOnce2 (ruleSomewhere merge), ruleOnce cancelTerms , ruleOnce2 distribute
   , ruleOnce2 (ruleSomewhere distributionSquare), ruleOnce flipEquation 
   , ruleOnce moveToLeft
   ]
   
higherDegreeRules :: [Rule (OrList (Equation Expr))]
higherDegreeRules = 
   [ allPowerFactors, ruleOnce2 powerFactor, sameFactor
   ] ++ quadraticRules

------------------------------------------------------------
-- General form rules: ax^2 + bx + c = 0

-- ax^2 + bx = 0 
noConFormula :: Rule (Equation Expr) 
noConFormula = makeSimpleRule "no constant c" $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (x, (a, b, c)) <- match (polyNormalForm rationalView >>> second quadraticPolyView) lhs
   guard (c == 0 && b /= 0)
   -- also search for constant factor
   let d = gcdFrac a b
   return (fromRational d .*. Var x .*. (fromRational (a/d) .*. Var x .+. fromRational (b/d)) :==: 0)

-- ax^2 + c = 0
noLinFormula :: Rule (Equation Expr)
noLinFormula = makeSimpleRule "no linear term b" $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (x, (a, b, c)) <- match (polyNormalForm rationalView >>> second quadraticPolyView) lhs
   guard (b == 0 && c /= 0)
   return $ 
      if a>0 then fromRational a .*. (Var x .^. 2) :==: fromRational (-c)
             else fromRational (-a) .*. (Var x .^. 2) :==: fromRational c

-- search for (X+A)*(X+B) decomposition 
niceFactors :: Rule (Equation Expr)
niceFactors = makeSimpleRule "nice factors" $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   let sign t@(x, (a, b, c)) = if a== -1 then (x, (1, -b, -c)) else t 
   (x, (a, rb, rc)) <- liftM sign (match (polyNormalForm rationalView >>> second quadraticPolyView) lhs)
   guard (a==1)
   b <- isInt rb
   c <- isInt rc
   case [ (Var x + fromInteger i) * (Var x + fromInteger j) | (i, j) <- factors c, i+j == b ] of
      hd:_ -> return (hd :==: 0)
      _    -> Nothing

simplerA :: Rule (Equation Expr)
simplerA = makeSimpleRule "simpler A" $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (x, (ra, rb, rc)) <- match (polyNormalForm rationalView >>> second quadraticPolyView) lhs
   [a, b, c] <- mapM isInt [ra, rb, rc] 
   let d = a `gcd` b `gcd` c
   guard (d `notElem` [0, 1])
   return (build quadraticView (x, fromInteger (a `div` d), fromInteger (b `div` d), fromInteger (c `div` d)) :==: 0)

abcFormula :: Rule (OrList (Equation Expr))
abcFormula = makeSimpleRule "abc formula" $ onceJoinM $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (x, (a, b, c)) <- match (polyNormalForm rationalView >>> second quadraticPolyView) lhs
   let discr = makeSqrt (fromRational (b*b - 4 * a * c))
   return $ orList
      [ Var x :==: (-fromRational b + discr) / (2 * fromRational a)
      , Var x :==: (-fromRational b - discr) / (2 * fromRational a)
      ]

------------------------------------------------------------
-- General form rules: expr = 0

mulZero :: Rule (OrList (Equation Expr))
mulZero = makeSimpleRule "multiplication is zero" $ onceJoinM $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (_, xs) <- match productView lhs
   guard (length xs > 1)
   return (orList [ x :==: 0 | x <- xs ])

------------------------------------------------------------
-- Constant form rules: expr = constant
 
coverUpPlus :: Rule (Equation Expr) 
coverUpPlus = coverUpPlusWith $ Config
   { configName        = "one var"
   , predicateCovered  = (==1) . length . collectVars
   , predicateCombined = noVars
   }

------------------------------------------------------------
-- Top form rules: expr1 = expr2

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
distributionSquare = makeSimpleRule "distribution square" f
 where
   f (Sym s [a :+: b, Nat 2]) | s == powerSymbol = do
      return ((a .^. 2) .+. (2 .*. a .*. b) + (b .^. 2))
   f (Sym s [a :-: b, Nat 2]) | s == powerSymbol = do
      return ((a .^. 2) .-. (2 .*. a .*. b) + (b .^. 2))
   f _ = Nothing

flipEquation :: Rule (Equation Expr)
flipEquation = makeSimpleRule "flip equation" $ \(lhs :==: rhs) -> do
   guard (hasVars rhs && noVars lhs)
   return (rhs :==: lhs)

moveToLeft :: Rule (Equation Expr)
moveToLeft = makeSimpleRule "move to left" $ \(lhs :==: rhs) -> do
   guard (rhs /= 0)
   let complex = case fmap (filter hasVars) $ match sumView (applyD merge lhs) of
                    Just xs | length xs >= 2 -> True
                    _ -> False
   guard (hasVars lhs && (hasVars rhs || complex))
   return (lhs - rhs :==: 0)

------------------------------------------------------------
-- Helpers and Rest

makeSqrt :: Expr -> Expr
makeSqrt (Nat n) | a*a == n = Nat a
 where a = SQ.isqrt n
makeSqrt e = sqrt e

factors :: Integer -> [(Integer, Integer)]
factors n = concat [ [(a, b), (negate a, negate b)] | a <- [1..h], let b = n `div` a, a*b == n ]
 where h = floor (sqrt (abs (fromIntegral n)))

isInt :: Rational -> Maybe Integer
isInt r = do
   guard (denominator r == 1)
   return (numerator r)

gcdFrac :: Rational -> Rational -> Rational
gcdFrac r1 r2 = fromMaybe 1 $ do 
   a <- isInt r1
   b <- isInt r2
   return (fromInteger (gcd a b))

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

-- Factor-out variable
powerFactor :: Rule Expr
powerFactor = makeSimpleRule "power factor" $ \e -> do
   xs <- match sumView e >>= mapM (match powerFactorView)
   let (vs, as, ns) = unzip3 xs
       r = minimum ns
       v = Var (head vs)
       f a n = a*v^fromIntegral (n-r)
   unless (length xs > 1 && length (nub vs) == 1 && r >= 1) Nothing
   -- also search for gcd constant
   case mapM (match integerView) as of 
      Just is | g > 1 -> 
         return (fromInteger g * v^fromIntegral r * foldr1 (+) (zipWith f (map (fromIntegral . (`div` g)) is) ns))
       where g = foldr1 gcd is
      _ -> 
         return (v^fromIntegral r * build sumView (zipWith f as ns))

-- A*B = A*C  implies  A=0 or B=C
sameFactor :: Rule (OrList (Equation Expr))
sameFactor = makeSimpleRule "same factor" $ onceJoinM $ \(lhs :==: rhs) -> do
   (b1, xs) <- match productView lhs
   (b2, ys) <- match productView rhs
   (x, y) <- safeHead [ (x, y) | x <- xs, y <- ys, x==y, hasVars x ] -- equality is too strong?
   return $ orList [ x :==: 0, build productView (b1, xs\\[x]) :==: build productView (b2, ys\\[y]) ]
   

---------------------------------------------------------
-- From LinearEquations

-------------------------------------------------------
-- Transformations

plusT, minusT :: Expr -> Transformation (Equation Expr)
plusT  e = makeTrans "plus"  $ return . fmap (applyD mergeT . (.+. e))
minusT e = makeTrans "minus" $ return . fmap (applyD mergeT . (.-. e))

timesT :: Expr -> Transformation (Equation Expr)
timesT e = makeTrans "times" $ \eq -> do 
   r <- match rationalView e
   guard (r /= 0)
   return $ fmap (applyD mergeT . applyD distributionT . (e .*.)) eq

divisionT :: Expr -> Transformation (Equation Expr)
divisionT e = makeTrans "division" $ \eq -> do
   r <- match rationalView e
   guard (r /= 0)
   return $ fmap (applyD mergeT . applyD distributionT . (./. e)) eq

distributionT :: Transformation Expr
distributionT = makeTrans "distribute" f 
 where
   f (a :*: b) =
      case (match sumView a, match sumView b) of
         (Just as, Just bs) | length as > 1 || length bs > 1 -> 
            return $ build sumView [ (a .*. b) | a <- as, b <- bs ]
         _ -> Nothing
   f _ = Nothing

mergeT :: Transformation Expr
mergeT = makeTrans "merge" $ return . collectLikeTerms

-------------------------------------------------------
-- Rewrite Rules

varToLeft :: Rule (Equation Expr)
varToLeft = makeRule "variable to left" $ flip supply1 minusT $ \eq -> do
   (x, a, _) <- match (linearViewWith rationalView) (getRHS eq)
   guard (a/=0)
   return (fromRational a * Var x)

conToRight :: Rule (Equation Expr)
conToRight = makeRule "constant to right" $ flip supply1 minusT $ \eq -> do
   (_, _, b) <- match (linearViewWith rationalView) (getLHS eq)
   guard (b/=0)
   return (fromRational b)

scaleToOne :: Rule (Equation Expr)
scaleToOne = makeRule "scale to one" $ flip supply1 divisionT $ \eq -> do
   (_, a, _) <- match (linearViewWith rationalView) (getLHS eq)
   guard (a `notElem` [0, 1])
   return (fromRational a)

removeDivision :: Rule (Equation Expr)
removeDivision = makeRule "remove division" $ flip supply1 timesT $ \(lhs :==: rhs) -> do
   xs <- match sumView lhs
   ys <- match sumView rhs
   zs <- mapM (fmap snd . match productView) (filter hasVars (xs ++ ys))
   let f = fmap snd . match (divView >>> second integerView)
   case catMaybes (map f (concat zs)) of
      [] -> Nothing
      ns -> return (fromInteger (foldr1 lcm ns))
   
distribute :: Rule Expr
distribute = makeSimpleRuleList "distribution" $
   somewhereM (\x -> applyM distributionT x >>= applyM mergeT)

merge :: Rule Expr
merge = makeSimpleRule "merge similar terms" $ \old -> do
   new <- apply mergeT old
   guard (old /= new)
   return new