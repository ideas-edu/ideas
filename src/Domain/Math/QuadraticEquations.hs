module Domain.Math.QuadraticEquations where

import Prelude hiding (repeat)
import Domain.Math.Equation
import Domain.Math.Expr
import Domain.Math.ExercisesDWO
import Domain.Math.LinearEquations (liftF, solvedEquation, minusT, timesT, divisionT, solveEquation)
import Domain.Math.Fraction (cleanUpStrategy)
import Domain.Math.Views
import Common.Apply
import Common.Context
import Common.Transformation
import Common.Strategy
import Test.QuickCheck hiding (label)
import Control.Monad
import Data.List (intersperse, (\\))
import Data.Maybe
import Data.Ratio
import qualified Data.IntMap as IM
import Debug.Trace

------------------------------------------------------------
-- OrList

newtype OrList a = OrList [a] deriving (Ord, Eq)

instance Functor OrList where
   fmap f (OrList xs) = OrList (map f xs)

instance Arbitrary a => Arbitrary (OrList a) where
   arbitrary = do 
      n  <- choose (1, 3)
      xs <- vector n
      return (OrList xs)
   coarbitrary (OrList xs) = coarbitrary xs

instance Show a => Show (OrList a) where
   show (OrList xs) 
      | null xs   = "true"
      | otherwise = unwords (intersperse "or" (map show xs))

solved :: OrList (Equation Expr) -> Bool
solved (OrList xs) = all solvedEquation xs

------------------------------------------------------------
-- Strategy and lifting

testje = traceStrategy (unlabel solverQ) $ 
   OrList [concat quadraticEquations !! 3]

solve :: Equation Expr -> OrList (Equation Expr)
solve e = applyD solverQ (OrList [e])

solverQ :: LabeledStrategy (OrList (Equation Expr))
solverQ = cleanUpStrategy cleanUpOrs $
   label "Quadratic equations" $
      repeat (coverUpPlus <|> coverUpTimes <|> coverUpNegate <|> coverUpSquare
         <|> coverUpDiv <|> cancelTerms <|> factorPower <|> mulZero
         <|> flipEquation) --  <|> conToRight <|> divideByConstant <|> linS)

cleanUpOrs :: OrList (Equation Expr) -> OrList (Equation Expr)
cleanUpOrs (OrList xs) = OrList (map (fmap simplifyExpr) xs)

linS :: Rule (OrList (Equation Expr))
linS = makeSimpleRuleList "linear equation" $ forOne $ \eq -> do
   match equationView eq
   cnew <- apply solveEquation (inContext eq)
   let new = fromContext cnew
   guard (new /= eq)
   return [new]
   
forOne :: (a -> Maybe [a]) -> OrList a -> [OrList a]
forOne f (OrList xs) = map OrList (rec xs)
 where
   rec []     = []
   rec (x:xs) = maybe [] (\y -> [y++xs]) (f x) ++ map (x:) (rec xs)

------------------------------------------------------------
-- Rules

makeSqrt :: Expr -> Expr
makeSqrt (Nat n) | a*a == n = Nat a
 where a = round (sqrt (fromInteger n))
makeSqrt e = sqrt e

-- X^2 = A  implies  X= +/- sqrt(A)
coverUpSquare :: Rule (OrList (Equation Expr))
coverUpSquare = makeSimpleRuleList "cover-up square" (forOne f)
 where
   f (Sym "^" [a, Nat 2] :==: rhs) | hasVars a && noVars rhs = do
      let e = makeSqrt rhs
      return [a :==: e, a :==: negate e]
   f _ = Nothing

{-
-- Under specific conditions, a constant should be moved to the right
conToRight :: Rule (OrList (Equation Expr))
conToRight = makeSimpleRuleList "constant to right" (forOne (fmap return . apply f))
 where
   f = flip supply1 minusT $ \(lhs :==: rhs) -> do
      guard (noVars rhs)
      (_, im) <- match polynomialView lhs
      case IM.toList im of
         [(0, c), (2, _)] -> canonical rationalView c
         _ -> Nothing

divideByConstant :: Rule (OrList (Equation Expr))
divideByConstant = makeSimpleRuleList "divide by constant" (forOne (fmap return . apply f))
 where
   f = flip supply1 divisionT $ \(lhs :==: rhs) -> do
      (bx, xs) <- match productView lhs
      (by, ys) <- match productView rhs
      let f = product . catMaybes . map (match rationalView)
          c = sign (f xs `smart` f ys)
          sign = if bx && noVars rhs || by && noVars lhs
                 then negate else id
      guard (c `notElem` [0, 1])
      return (fromRational c)

   smart :: Rational -> Rational -> Rational
   smart r1 r2
      | denominator r1 == 1 && denominator r2 == 1 =
           fromIntegral (numerator r1 `gcd` numerator r2)
      | otherwise =
           1 / (fromIntegral (denominator r1 `lcm` denominator r2)) -}

coverUpPlus :: Rule (OrList (Equation Expr))
coverUpPlus = makeSimpleRuleList "cover-up plus" (forOne (fmap return . apply f))
 where
   f = flip supply1 minusT $ \(lhs :==: rhs) -> do
      guard (noVars rhs)
      (a, b) <- match plusView lhs
      r <- case (match rationalView a, match rationalView b) of
              (Just r, _) | hasVars b -> Just r
              (_, Just r) | hasVars a -> Just r
              _                       -> Nothing
      guard (r /= 0)
      return (build rationalView r)

coverUpTimes :: Rule (OrList (Equation Expr))
coverUpTimes = makeSimpleRuleList "cover-up times" (forOne (fmap return . apply f))
 where
   f = flip supply1 divisionT $ \(lhs :==: rhs) -> do
      guard (noVars rhs)
      (a, b) <- match timesView lhs
      r <- case (match rationalView a, match rationalView b) of
              (Just r, _) | hasVars b -> Just r
              (_, Just r) | hasVars a -> Just r
              _                       -> Nothing
      guard (r `notElem` [0, 1])
      return (build rationalView r)

coverUpNegate :: Rule (OrList (Equation Expr))
coverUpNegate = makeSimpleRuleList "cover-up negate" (forOne f)
 where
   f (Negate a :==: b) | hasVars a && noVars b = 
      return [a :==: Negate b]
   f (a :==: Negate b) | noVars a && hasVars b =
      return [Negate a :==: b]
   f _ = Nothing

coverUpDiv :: Rule (OrList (Equation Expr))
coverUpDiv = makeSimpleRuleList "cover-up division" (forOne (fmap return . apply f))
 where
   f = flip supply1 timesT $ \(lhs :==: rhs) -> do
      guard (noVars rhs)
      (a, b) <- match divView lhs
      guard (hasVars a)
      r <- match rationalView b
      guard (r `notElem` [0, 1])
      return (build rationalView r)

cancelTerms :: Rule (OrList (Equation Expr))
cancelTerms = makeSimpleRuleList "cancel terms" $ forOne $ \(lhs :==: rhs) -> do
   xs <- match sumView lhs
   ys <- match sumView rhs
   let without a as = build sumView (as \\ [a])
   case [ x | x <- xs, y <- ys, x==y ] of
      []   -> Nothing
      hd:_ -> return [without hd xs :==: without hd ys]

-- x^n == x^m   or    x^n+x^m ==0    (can this be combined?)
factorPower :: Rule (OrList (Equation Expr))
factorPower = makeSimpleRuleList "factor power" $ forOne $ \(lhs :==: rhs) -> do
   (e1, x1, n1) <- match powerView lhs
   (e2, x2, n2) <- match powerView rhs
   guard (x1==x2 && n1 > 0 && n2 > 0)
   let m = n1 `min` n2
       make e n = build powerView (e, x1, n-m)
   return [ Var x1 :==: 0, make e1 n1 :==: make e2 n2 ]
 `mplus` do
   guard (rhs == 0)
   (a, b) <- match plusView lhs
   (e1, x1, n1) <- match powerView a
   (e2, x2, n2) <- match powerView b
   guard (x1==x2 && n1 > 0 && n2 > 0)
   let m = n1 `min` n2
       make e n = build powerView (e, x1, n-m)
   return [ Var x1 :==: 0, make e1 n1 .+. make e2 n2 :==: 0 ]

mulZero :: Rule (OrList (Equation Expr))
mulZero = makeSimpleRuleList "multiplication is zero" $ forOne $ \(lhs :==: rhs) -> do
   (_, xs) <- match productView lhs
   guard (rhs == 0 && length xs > 1)
   return [ x :==: 0 | x <- xs ]

-- really needed?
flipEquation :: Rule (OrList (Equation Expr))
flipEquation = makeSimpleRuleList "flip equation" $ forOne $ \(lhs :==: rhs) -> do
   guard (noVars lhs && hasVars rhs)
   return [ rhs :==: lhs ]

------------------------------------------------------------
-- Testing

main = zipWith f [1..] (concat quadraticEquations)
 where
   f i e = if solved (solve e) then i else error (show e ++ "  becomes   " ++ show (solve e))















  -- (higherDegreeEquationExercise, main) where

{-
import Prelude hiding ((^), repeat)
import Data.List (nub, sort, (\\), intersperse)
import Data.Maybe
import Common.Context
import Common.Exercise
import qualified Common.Parsing as P
import Common.Utils (safeHead, fixpoint)
import Common.Transformation
import Common.Strategy hiding (not)
import Common.Uniplate
import Domain.Math.ExercisesDWO (higherDegreeEquations)
import Domain.Math.LinearEquations (showDerivation, showDerivations, solvedEquation)

import Domain.Math.Parser
import Domain.Math.Symbolic
import Domain.Math.Fraction (cleanUpStrategy)
import Domain.Math.Views

import Control.Monad 
import Test.QuickCheck hiding (check, label)

------------------------------------------------------------
-- Exercise

higherDegreeEquationExercise :: Exercise (OrList (Equation Expr))
higherDegreeEquationExercise = makeExercise 
   { identifier    = "higherdegree"
   , domain        = "math"
   , description   = "solve an equation (higher degree)"
   , status        = Experimental
   , parser        = parseOrs
   , equality      = (==) 
                     -- let f (OrList xs) = sort (map (fmap cleanUpExpr) xs)
                     -- in \a b -> a == b
   , equivalence   = \_ _ -> True -- equality higherDegreeEquationExercise -- TO DO: what about equivalence for undecidable domains?
   , finalProperty = solved
   , ruleset       = allRules
   , strategy      = equationsStrategy
   , generator     = oneof (map (return . OrList . return) $ take 1 higherDegreeEquations)
   }
   
parseOrs :: String -> Either P.SyntaxError (OrList (Equation Expr))
parseOrs = f . P.parse pOrs . P.scanWith myScanner
 where 
   myScanner = scannerExpr 
      { P.keywordOperators = "==" : P.keywordOperators scannerExpr 
      , P.keywords = ["or"]
      }
   
   pOrs = pSepList (pEquation pExpr) (P.pKey "or")
   pSepList p q = (\x xs -> OrList (x:xs)) P.<$> p P.<*> P.pList (q P.*> p)
 
   f (e, []) = Right e
   f (_, xs) = Left $ P.ErrorMessage $ unlines $ map show xs

--------------------


 
-----------------------------------------------------------
      
-- A*B = 0  implies  A=0 or B=0
productZero :: Equation Expr -> Maybe [Equation Expr]
productZero (e :==: Nat 0) | length xs > 1 = 
   return [ x :==: 0 | x <- xs ]
 where xs = maybe [] snd (match productView e)
productZero _ = Nothing

-- A^B = 0  implies  A=0
powerZero :: Equation Expr -> Maybe (Equation Expr)
powerZero (Sym "^" [a, _] :==: Nat 0) =
   return (a :==: 0)
powerZero _ = Nothing

-- A*X^I + B = C  implies  A*X^I = C - B
-- copy/paste, but it is just for demonstration
moveToRHS :: Equation Expr -> Maybe (Equation Expr)
moveToRHS (lhs :==: rhs) = do
   xs <- match sumView lhs
   unless (length xs > 1) Nothing
   case filter hasVars xs of
      [e] -> return (e :==: rhs - foldr1 (+) (xs \\ [e]))
      _   -> Nothing

powerFactor :: Expr -> Maybe Expr
powerFactor e = do
   xs <- match sumView e >>= mapM (match powerView)
   let (as, vs, ns) = unzip3 xs
       r = minimum ns
       v = variable (head vs)
       f a n = a*v^(Nat (n-r))
       Nat n = if n < 0 then negate (Nat (abs n)) else Nat n
   unless (length xs > 1 && length (nub vs) == 1 && r >= 1) Nothing
   -- also search for gcd constant
   case mapM (match integerView) as of 
      Just is | g > 1 -> 
         return (Nat g * v^Nat r * foldr1 (+) (zipWith f (map (Nat . (`div` g)) is) ns))
       where g = foldr1 gcd is
      _ -> 
         return (v^Nat r * foldr1 (+) (zipWith f as ns))



-- A*X^i  = B  implies  X^i = B/A
divide :: Equation Expr -> Maybe (Equation Expr)
divide (lhs :==: rhs) = do
   unless (noVars rhs) Nothing
   (a, x, i) <- match powerView lhs
   when (a == 1 || i == 0) Nothing
   return (variable x ^ Nat i :==: rhs / a) 

-- search for (X+A)*(X+B) decomposition 
niceFactors :: Expr -> Maybe Expr
niceFactors e = do
   (x, a, b, c) <- match quadraticView e
   unless (a==1) Nothing
   safeHead [ (variable x + Nat i) * (variable x + Nat j) | (i, j) <- factors c, i+j == b ]

moveToLHS :: Equation Expr -> Maybe (Equation Expr)
moveToLHS (x :==: y) = do 
   (_, _, n) <- match powerView y
   unless (n >= 1) Nothing
   return (x - y :==: 0)

abcFormula :: Equation Expr -> Maybe [Equation Expr]
abcFormula (e :==: Nat 0) = do
   (x, a, b, c) <- match quadraticView e
   let discr = Nat (b*b - 4 * a * c)
   return [ variable x :==: (-Nat b + sqrt discr) / 2 * Nat a
          , variable x :==: (-Nat b - sqrt discr) / 2 * Nat a
          ]
abcFormula _ = Nothing

-- A*B = A*C  implies  A=0 or B=C
sameFactor :: Equation Expr -> Maybe [Equation Expr]
sameFactor (lhs :==: rhs) = do
   (b1, xs) <- match productView lhs
   (b2, ys) <- match productView rhs
   (x, y) <- safeHead [ (x, y) | x <- xs, y <- ys, x==y ] -- equality is too strong?
   return [ x :==: 0, build productView (b1, xs\\[x]) :==: build productView (b2, ys\\[y]) ]

-----------------------

rule1, rule2, rule3, rule4, rule5, rule6, rule7, rule8, rule9, rule10
   :: Rule (OrList (Equation Expr))
rule1  = makeSimpleRule "productZero" $ liftEqn productZero
rule2  = makeSimpleRule "powerZero"   $ liftEqn (fmap return . powerZero)
rule3  = makeSimpleRule "moveToRHS"   $ liftEqn (fmap return . moveToRHS)
rule4  = makeSimpleRule "powerFactor" $ liftExpr powerFactor
rule5  = makeSimpleRule "squared"     $ liftEqn squared
rule6  = makeSimpleRule "niceFactors" $ liftExpr niceFactors
rule7  = makeSimpleRule "moveToLHS"   $ liftEqn (fmap return . moveToLHS)
rule8  = makeSimpleRule "divide"      $ liftEqn (fmap return . divide)
rule9  = makeSimpleRule "abcFormula"  $ liftEqn abcFormula
rule10 = makeSimpleRule "sameFactor"  $ liftEqn sameFactor

liftEqn :: (Equation Expr -> Maybe [Equation Expr]) -> (OrList (Equation Expr) -> Maybe (OrList (Equation Expr)))
liftEqn f (OrList eqs) = fmap OrList (rec eqs) 
 where
   rec []     = Nothing
   rec (x:xs) = 
      case f x of
         Just ys -> return (ys ++ xs)
         Nothing -> fmap (x:) (rec xs)

liftExpr :: (Expr -> Maybe Expr) -> (OrList (Equation Expr) -> Maybe (OrList (Equation Expr)))
liftExpr f = liftEqn $ \(lhs :==: rhs) ->
   case ({- somewhereM -} f lhs, {- somewhereM -} f rhs) of
      (Just n, _) -> Just [n :==: rhs]
      (_, Just n) -> Just [lhs :==: n]
      _           -> Nothing

-----------------------

-- clean up the expression: not at all a complete list of simplifications
cleanUpExpr :: Expr -> Expr 
cleanUpExpr = fixpoint (transform (\e -> fromMaybe (step e) (basic e)))
 where
   -- plus
   step (x :+: Negate y) = x - y
   step (x :+: (Negate y :*: z)) = x - (y*z)
   -- minus
   step (x :-: (y :-: z)) = (x-y)+z
   step (0 :-: x) = Negate x
   -- negate 
   step (Negate (Nat 0))   = 0
   step (Negate (x :*: y)) = (-x)*y
   -- times
   step (x :*: (y :*: z)) | noVars x && noVars y = ((x :*: y) :*: z) -- not so nice!
   step ((x :/: y) :*: z) | y==z = x
   step (x :*: (y :/: z)) | x==z = y
   step (x :*: (y :/: z)) = (x :*: y) :/: z
   -- division
   step ((a :*: x) :/: b) | a==b && b/=0 = x
   step (Negate (x :/: y)) = (-x)/y
   -- square roots
   step (Sqrt (Nat n)) | a*a == n = Nat a
    where a = round (sqrt (fromIntegral n))
   step (Sqrt (x@(Nat _) :/: y)) = sqrt x / sqrt y -- exceptional case: sqrt (-2/-3)
   step (Sqrt (x :/: y@(Nat _))) = sqrt x / sqrt y
   -- finally, propagate constants
   step expr =
      let Nat n = if n >= 0 then Nat n else negate (Nat (abs n)) in
      case match rationalView expr of
         Just r -> fromRational r
         Nothing -> expr

   -- identities/absorbing
   basic :: Expr -> Maybe Expr
   basic (Nat 0 :+: x) = return x
   basic (x :+: Nat 0) = return x
   basic (Nat 0 :*: _) = return 0
   basic (_ :*: Nat 0) = return 0
   basic (Nat 1 :*: x) = return x
   basic (x :*: Nat 1) = return x
   basic (x :-: Nat 0) = return x
   basic (x :/: Nat 1) = return x
   basic (Nat 0 :/: _) = return 0 -- division-by-zero
   basic (Sym "^" [_, Nat 0]) = return 1
   basic (Sym "^" [x, Nat 1]) = return x
   basic (Sym "^" [Nat 0, _]) = return 0 -- except that 0^0 = 1 (by definition), or left undefined
   basic (Sym "^" [Nat 1, _]) = return 1
   basic _ = Nothing
   
-----------------------

-- The last check "removes" the unsuccessful paths. Quick solution
-- to get the derivations, but does not prevent these paths from
-- being explored by the "first" service
equationsStrategy :: LabeledStrategy (Context (OrList (Equation Expr)))
equationsStrategy = cleanUpStrategy (fmap (fmap (fmap cleanUpExpr))) $
   label "higher degree" $ repeat ( 
   alternatives allRules)
        -- <*> check (solved . fromContext)
   
allRules :: [Rule (Context (OrList (Equation Expr)))]
allRules = map liftRule [ rule1, rule2, rule3, rule4, rule5
                        , rule6, rule7, rule8, rule10, rule9] --abc last in list
 
liftRule :: Rule a -> Rule (Context a)
liftRule = lift $ makeLiftPair (return . fromContext) (fmap . const)

test n = showDerivations (unlabel equationsStrategy) 
   (inContext (OrList [higherDegreeEquations !! (n-1)]))

factors :: Integer -> [(Integer, Integer)]
factors n = concat [ [(a, b), (negate a, negate b)] | a <- [1..h], let b = n `div` a, a*b == n ]
 where h = floor (sqrt (abs (fromIntegral n)))
 
q = traceStrategy (unlabel equationsStrategy) 
       (inContext (OrList [ (x-1)*(x^3 - 6*x) :==: 3*x^3 - 3*x^2]))
                                  -- (x-1)*(x^3 - 6*x) :==: 3*x^2 * (x-1) ])
 where x = variable "x"
       
main :: IO ()
main = flip mapM_ [1..10] $ \i -> do
   let line  = putStrLn (replicate 50 '-')  
       start = inContext (OrList [higherDegreeEquations !! (i-1)])
   {- line
   putStrLn $ "Exercise " ++ show i
   line -} 
   case derivations (unlabel equationsStrategy) start of
      hd:_ -> putStrLn "ok" --showDerivation "" hd
      _    -> putStrLn "unsolved" -}