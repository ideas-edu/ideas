module Domain.Math.HigherDegreeEquations where
  -- (higherDegreeEquationExercise, main) where

import Prelude hiding ((^), repeat)
import Data.List (nub, sort, sortBy, (\\), intersperse)
import Data.Maybe
import Data.Ratio
import Common.Context
import Common.Exercise
import qualified Common.Parsing as P
import Common.Utils (safeHead, fixpoint)
import Common.Transformation
import Common.Strategy hiding (not)
import Common.Uniplate
import Domain.Math.ExercisesDWO (higherDegreeEquations)
import Domain.Math.Expr
import Domain.Math.Parser
import Domain.Math.Symbolic
import Domain.Math.Fraction (cleanUpStrategy)
import Domain.Math.Views
import Domain.Math.Equation
import Control.Monad (unless, when)
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
   , equality      = let f (OrList xs) = sort (map (fmap cleanUpExpr) xs)
                     in \a b -> f a == f b
   , equivalence   = equality higherDegreeEquationExercise -- TO DO: what about equivalence for undecidable domains?
   , finalProperty = solved
   , ruleset       = allRules
   , strategy      = equationsStrategy
   , generator     = oneof (take 9 $ map (return . OrList . return) higherDegreeEquations)
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

solvedEquation :: Equation Expr -> Bool
solvedEquation (lhs :==: rhs) =
   case lhs of 
      Var _ -> noVars rhs
      _     -> False
 
-----------------------------------------------------------
-- Views on mathematical expressions

-- e.g., 3 or -3
constantView :: Expr -> Maybe Integer
constantView = exprToNum -- match integerView

ratioView :: Expr -> Maybe (Integer, Integer)
ratioView = fmap (\r -> (numerator r, denominator r)) . exprToFractional

-- a*x^b, e.g., -3*x^2, but also 3*x, x, and 3
powerView :: Expr -> Maybe (Expr, String, Integer)
powerView e | isJust (ratioView e) = return (e, "x", 0) -- guess variable !!!!!!! 
powerView (Negate a) = do
   (e, x, n) <- powerView a
   return (Negate e, x, n)
powerView (Var x) = return (1, x, 1)
powerView (Sym "^" [Var x, Nat n]) =
   return (1, x, n)
powerView (a :*: b)
   | hasVars a && not (hasVars b) = do
        (e, x, n) <- powerView a
        return (b*e, x, n)
   | not (hasVars a) && hasVars b = do
        (e, x, n) <- powerView b
        return (a*e, x, n)
powerView _ = Nothing

-- a*x^2 + b*x + c
quadraticView :: Expr -> Maybe (String, Integer, Integer, Integer)
quadraticView e = do 
   xs <- match sumView e >>= mapM powerView
   let vs = nub [ x | (_, x, _) <- xs ]
       cmp (_, _, x) (_, _, y) = compare x y
   unless (length vs == 1) Nothing
   case sortBy cmp xs of
      [(e0, _, 0), (e1, _, 1), (e2, _, 2)] -> do
         a <- constantView e2
         b <- constantView e1
         c <- constantView e0
         return (head vs, a, b, c)
      _ -> Nothing
      
-----------------------------------------------------------
      
-- A*B = 0  implies  A=0 or B=0
productZero :: Equation Expr -> Maybe [Equation Expr]
productZero (e :==: Nat 0) | length xs > 1 = 
   return [ x :==: 0 | x <- xs ]
 where xs = fromMaybe [] (match productView e)
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
   xs <- match sumView e >>= mapM powerView
   let (as, vs, ns) = unzip3 xs
       r = minimum ns
       v = variable (head vs)
       f a n = a*v^(Nat (n-r))
       Nat n = if n < 0 then negate (Nat (abs n)) else Nat n
   unless (length xs > 1 && length (nub vs) == 1 && r >= 1) Nothing
   -- also search for gcd constant
   case mapM constantView as of 
      Just is | g > 1 -> 
         return (Nat g * v^Nat r * foldr1 (+) (zipWith f (map (Nat . (`div` g)) is) ns))
       where g = foldr1 gcd is
      _ -> 
         return (v^Nat r * foldr1 (+) (zipWith f as ns))

-- X^2 = A  implies  X= +/- sqrt(A)
squared :: Equation Expr -> Maybe [Equation Expr]
squared (Sym "^" [Var x, Nat 2] :==: c) | noVars c = 
   return [Var x :==: sqrt c, Var x :==: negate (sqrt c)]
squared _ = Nothing

-- A*X^i  = B  implies  X^i = B/A
divide :: Equation Expr -> Maybe (Equation Expr)
divide (lhs :==: rhs) = do
   unless (noVars rhs) Nothing
   (a, x, i) <- powerView lhs
   when (a == 1 || i == 0) Nothing
   return (variable x ^ Nat i :==: rhs / a) 

-- search for (X+A)*(X+B) decomposition 
niceFactors :: Expr -> Maybe Expr
niceFactors e = do
   (x, a, b, c) <- quadraticView e
   unless (a==1) Nothing
   safeHead [ (variable x + Nat i) * (variable x + Nat j) | (i, j) <- factors c, i+j == b ]

moveToLHS :: Equation Expr -> Maybe (Equation Expr)
moveToLHS (x :==: y) = do 
   (_, _, n) <- powerView y
   unless (n >= 1) Nothing
   return (x - y :==: 0)

abcFormula :: Equation Expr -> Maybe [Equation Expr]
abcFormula (e :==: Nat 0) = do
   (x, a, b, c) <- quadraticView e
   let discr = Nat (b*b - 4 * a * c)
   return [ variable x :==: (-Nat b + sqrt discr) / 2 * Nat a
          , variable x :==: (-Nat b - sqrt discr) / 2 * Nat a
          ]
abcFormula _ = Nothing

-- A*B = A*C  implies  A=0 or B=C
sameFactor :: Equation Expr -> Maybe [Equation Expr]
sameFactor (lhs :==: rhs) = do
   xs <- match productView lhs
   ys <- match productView rhs
   (x, y) <- safeHead [ (x, y) | x <- xs, y <- ys, x==y ] -- equality is too strong?
   return [ x :==: 0, foldr1 (*) (xs\\[x]) :==: foldr1 (*) (ys\\[y]) ]

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
      case ratioView expr of
         Just (a, b)
            | b==1      -> Nat a
            | otherwise -> Nat a / Nat b
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
   alternatives (allRules \\ [liftRule rule9])
        |> liftRule rule9) 
        <*> check (solved . fromContext)
   
allRules :: [Rule (Context (OrList (Equation Expr)))]
allRules = map liftRule [ rule1, rule2, rule3, rule4, rule5
                        , rule6, rule7, rule8, rule9, rule10]
 
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
 
showDerivations :: Show a => Strategy a -> a -> IO ()
showDerivations s a = mapM_ make list
 where
   list = zip [1..] (derivations s a)
   make (n, d) = showDerivation ("Derivation " ++ show n ++ ":") d

showDerivation :: Show a => String -> (a, [(Rule a, a)]) -> IO ()
showDerivation title (a, list) = 
   putStrLn $ unlines $ title : f a : concatMap g list
 where
   f a = "  " ++ show a
   g (r, a)
      | isMinorRule r = []
      | otherwise =  ["    => " ++ show r, f a]
      
main :: IO ()
main = flip mapM_ [1..10] $ \i -> do
   let line  = putStrLn (replicate 50 '-')  
       start = inContext (OrList [higherDegreeEquations !! (i-1)])
   {- line
   putStrLn $ "Exercise " ++ show i
   line -} 
   case derivations (unlabel equationsStrategy) start of
      hd:_ -> putStrLn "ok" -- showDerivation "" hd
      _    -> putStrLn "unsolved"