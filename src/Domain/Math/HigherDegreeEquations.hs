module Domain.Math.HigherDegreeEquations where

import Prelude hiding ((^), repeat)
import Data.List (nub, sortBy, (\\), intersperse)
import Data.Maybe
import Data.Ratio
import Common.Utils (safeHead, fixpoint)
import Common.Transformation
import Common.Strategy hiding (not)
import Common.Uniplate
import Domain.Math.Expr
import Domain.Math.Symbolic
import Domain.LinearAlgebra.Equation
import Control.Monad (unless, when)

newtype OrList a = OrList [a]

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
constantView (Con n)    = return n
constantView (Negate a) = fmap negate (constantView a)
constantView _          = Nothing

ratioView :: Expr -> Maybe (Integer, Integer)
ratioView = fmap (\r -> (numerator r, denominator r)) . exprToFractional

-- e.g., 3+4+5 or 3-4+5 or -(3+4)
sumView :: Expr -> [Expr]
sumView (a :+: b)  = sumView a ++ sumView b
sumView (a :-: b)  = sumView a ++ map negate (sumView b)
sumView (Negate a) = map negate (sumView a)
sumView expr       = [expr]

productView :: Expr -> [Expr] -- what about negate?
productView (a :*: b) = productView a ++ productView b
productView expr = [expr]

-- a*x^b, e.g., -3*x^2, but also 3*x, x, and 3
powerView :: Expr -> Maybe (Expr, String, Integer)
powerView e | isJust (ratioView e) = return (e, "x", 0) -- guess variable !!!!!!! 
powerView (Negate a) = do
   (e, x, n) <- powerView a
   return (Negate e, x, n)
powerView (Var x) = return (1, x, 1)
powerView (Sym "^" [Var x, Con n]) =
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
   xs <- mapM powerView (sumView e)
   let vs = nub [ x | (_, x, _) <- xs ]
       cmp (_, _, x) (_, _, y) = compare x y
   unless (length vs == 1) Nothing
   case sortBy cmp xs of
      [(e0, _, 0), (e1, _, 1), (e2, _, 2)] -> do
         a <- constantView (cleanup e2)
         b <- constantView (cleanup e1)
         c <- constantView (cleanup e0)
         return (head vs, a, b, c)
      _ -> Nothing
      
-----------------------------------------------------------
      
-- A*B = 0  implies  A=0 or B=0
productZero :: Equation Expr -> Maybe [Equation Expr]
productZero (e :==: Con 0) | length xs > 1 = 
   return [ x :==: 0 | x <- xs ]
 where xs = productView e
productZero _ = Nothing

-- A^B = 0  implies  A=0
powerZero :: Equation Expr -> Maybe (Equation Expr)
powerZero (Sym "^" [a, _] :==: Con 0) =
   return (a :==: 0)
powerZero _ = Nothing

-- A*X^I + B = C  implies  A*X^I = C - B
-- copy/paste, but it is just for demonstration
moveToRHS :: Equation Expr -> Maybe (Equation Expr)
moveToRHS (lhs :==: rhs) = do
   let xs = sumView lhs
   unless (length xs > 1) Nothing
   case filter hasVars xs of
      [e] -> return (e :==: rhs - foldr1 (+) (xs \\ [e]))
      _   -> Nothing

powerFactor :: Expr -> Maybe Expr
powerFactor e = do
   xs <- mapM powerView (sumView e)
   let (as, vs, ns) = unzip3 xs
       r = minimum ns
       v = variable (head vs)
       f a n = a*v^(Con (n-r))
       con n = if n < 0 then negate (Con (abs n)) else Con n
   unless (length xs > 1 && length (nub vs) == 1 && r >= 1) Nothing
   -- also search for gcd constant
   case mapM constantView (map cleanup as) of 
      Just is | g > 1 -> 
         return (Con g * v^Con r * foldr1 (+) (zipWith f (map (con . (`div` g)) is) ns))
       where g = foldr1 gcd is
      _ -> 
         return (v^Con r * foldr1 (+) (zipWith f as ns))

-- X^2 = A  implies  X= +/- sqrt(A)
squared :: Equation Expr -> Maybe [Equation Expr]
squared (Sym "^" [Var x, Con 2] :==: c) | noVars c = 
   return [Var x :==: sqrt c, Var x :==: negate (sqrt c)]
squared _ = Nothing

-- A*X^i  = B  implies  X^i = B/A
divide :: Equation Expr -> Maybe (Equation Expr)
divide (lhs :==: rhs) = do
   unless (noVars rhs) Nothing
   (a, x, i) <- powerView lhs
   when (a == 1 || i == 0) Nothing
   return (variable x ^ Con i :==: rhs / a) 

-- search for (X+A)*(X+B) decomposition 
niceFactors :: Expr -> Maybe Expr
niceFactors e = do
   (x, a, b, c) <- quadraticView e
   unless (a==1) Nothing
   safeHead [ (variable x + Con i) * (variable x + Con j) | (i, j) <- factors c, i+j == b ]

moveToLHS :: Equation Expr -> Maybe (Equation Expr)
moveToLHS (x :==: y) = do 
   (_, _, n) <- powerView y
   unless (n >= 1) Nothing
   return (x - y :==: 0)

abcFormula :: Equation Expr -> Maybe [Equation Expr]
abcFormula (e :==: Con 0) = do
   (x, a, b, c) <- quadraticView e
   let discr = Con (b*b - 4 * a * c)
   return [ variable x :==: (-Con b + sqrt discr) / 2 * Con a
          , variable x :==: (-Con b - sqrt discr) / 2 * Con a
          ]
abcFormula _ = Nothing

-- A*B = A*C  implies  A=0 or B=C
sameFactor :: Equation Expr -> Maybe [Equation Expr]
sameFactor (lhs :==: rhs) = do
   let xs = productView lhs
       ys = productView rhs
   (x, y) <- safeHead [ (x, y) | x <- xs, y <- ys, x==y ] -- equality is too strong?
   return [ x :==: 0, foldr1 (*) (xs\\[x]) :==: foldr1 (*) (ys\\[y]) ]

-----------------------

rule1, rule2, rule3, rule4, rule5, rule6, rule7, rule8, rule9, rule10
   :: Rule (OrList (Equation Expr))
rule1  = makeSimpleRule "productZero" $ liftEqn productZero
rule2  = makeSimpleRule "powerZero"   $ liftEqn (fmap return . powerZero)
rule3  = makeSimpleRule "moveToRHS"     $ liftEqn (fmap return . moveToRHS)
rule4  = makeSimpleRule "powerFactor" $ liftExpr powerFactor
rule5  = makeSimpleRule "squared"     $ liftEqn squared
rule6  = makeSimpleRule "niceFactors" $ liftExpr niceFactors
rule7  = makeSimpleRule "moveToLHS"   $ liftEqn (fmap return . moveToLHS)
rule8  = makeSimpleRule "divide"      $ liftEqn (fmap return . divide)
rule9  = makeSimpleRule "abcFormula"  $ liftEqn abcFormula
rule10 = makeSimpleRule "sameFactor"  $ liftEqn sameFactor

-- takes care of cleaning up afterwards
liftEqn :: (Equation Expr -> Maybe [Equation Expr]) -> (OrList (Equation Expr) -> Maybe (OrList (Equation Expr)))
liftEqn f (OrList eqs) = fmap OrList (rec eqs) 
 where
   rec []     = Nothing
   rec (x:xs) = 
      case f x of
         Just ys -> return (map (fmap cleanup) ys ++ xs)
         Nothing -> fmap (x:) (rec xs)

liftExpr :: (Expr -> Maybe Expr) -> (OrList (Equation Expr) -> Maybe (OrList (Equation Expr)))
liftExpr f = liftEqn $ \(lhs :==: rhs) ->
   case ({- somewhereM -} f lhs, {- somewhereM -} f rhs) of
      (Just n, _) -> Just [n :==: rhs]
      (_, Just n) -> Just [lhs :==: n]
      _           -> Nothing

-----------------------

-- clean up the expression: not at all a complete list of simplifications
cleanup :: Expr -> Expr 
cleanup = fixpoint (transform step)
 where
   -- plus
   step (Con 0 :+: x) = x
   step (x :+: Con 0) = x
   step (x :+: Negate y) = x - y
   step (x :+: (Negate y :*: z)) = x - (y*z)
   -- minus
   step (x :-: (y :-: z)) = (x-y)+z
   step (0 :-: x) = Negate x
   -- negate 
   step (Negate (Con 0))   = 0
   step (Negate (x :*: y)) = (-x)*y
   -- times
   step (Con 0 :*: _) = 0
   step (Con 1 :*: x) = x
   step (_ :*: Con 0) = 0
   step (x :*: Con 1) = x
   step (x :*: (y :*: z)) | noVars x && noVars y = ((x :*: y) :*: z) -- not so nice!
   step ((x :/: y) :*: z) | y==z = x
   step (x :*: (y :/: z)) | x==z = y
   step (x :*: (y :/: z)) = (x :*: y) :/: z
   -- division
   step ((a :*: x) :/: b) | a==b && b/=0 = x
   step (Negate (x :/: y)) = (-x)/y
   -- power
   step (Sym "^" [_, Con 0]) = 1
   step (Sym "^" [x, Con 1]) = x
   step (Sym "^" [Con 0, _]) = 0
   step (Sym "^" [Con 1, _]) = 1
   -- square roots
   step (Sqrt (Con n)) | a*a == n = Con a
    where a = round (sqrt (fromIntegral n))
   step (Sqrt (x@(Con _) :/: y)) = sqrt x / sqrt y -- exceptional case: sqrt (-2/-3)
   step (Sqrt (x :/: y@(Con _))) = sqrt x / sqrt y
   -- finally, propagate constants
   step expr =
      let con n = if n >= 0 then Con n else negate (Con (abs n)) in
      case ratioView expr of
         Just (a, b)
            | b==1      -> con a
            | otherwise -> con a / con b
         Nothing -> expr

-----------------------

solve :: Strategy (OrList (Equation Expr))
solve = repeat ( 
   alternatives [rule1, rule2, rule3, rule4, rule5, rule6, rule7, rule8, rule10]
        |> rule9) <*> check solved

examples :: [Equation Expr]
examples = 
   [ x^3 + x^2 :==: 0
   , x^3 - 5*x :==: 0
   , x^3 - 11*x^2 + 18*x :==: 0
   , x^3 + 36*x :==: 13*x^2
   , x^3 + 2*x^2 :==: 24*x
   , 7*x^3 :==: 8*x^2
   , x^4 :==: 9*x^2
   , 64*x^7 :==: x^5
   , x^3 - 4*x^2 - 9*x :==: 0
   , (x-1)*(x^3 - 6*x) :==: 3*x^3 - 3*x^2
   ]
 where x = variable "x"

test n = showDerivations solve (OrList [examples !! (n-1)])

factors :: Integer -> [(Integer, Integer)]
factors n = concat [ [(a, b), (negate a, negate b)] | a <- [1..h], let b = n `div` a, a*b == n ]
 where h = floor (sqrt (abs (fromIntegral n)))
 
q = traceStrategy solve (OrList [ (x-1)*(x^3 - 6*x) :==: 3*x^3 - 3*x^2])
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
       start = OrList [examples !! (i-1)]
   line
   putStrLn $ "Exercise " ++ show i
   line 
   case derivations solve start of
      hd:_ -> showDerivation "" hd
      _    -> putStrLn "unsolved"