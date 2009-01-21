module Domain.Math.LinearEquations where

import Prelude hiding ((^), repeat)
import Common.Utils (distinct)
import Common.Strategy
import Common.Transformation
import Common.Uniplate
import Domain.Math.Expr
import Domain.Math.Symbolic
import Domain.Math.HigherDegreeEquations (powerView, sumView, constantView, ratioView, cleanup, showDerivation) -- to reuse some helpers (temp)
import Domain.LinearAlgebra.Equation
import Control.Monad
import Data.List hiding (repeat)
import Data.Maybe
import Data.Ratio

-------------------------------------------------------
-- Views

-- ax + b
linearView :: Expr -> Maybe (Rational, String, Rational)
linearView e = do
   list <- mapM powerView (sumView e)
   let (as, xs, ns) = unzip3 list
   unless (length (nub xs) == 1 && distinct ns && all (<=1) ns) Nothing
   cs <- mapM ratioView (map cleanup as)
   let a = maybe 0 f $ lookup 1 (zip ns cs)
       b = maybe 0 f $ lookup 0 (zip ns cs)
       f (i,j) = fromIntegral i/fromIntegral j
   return (a, head xs, b)

fromRat :: Rational -> Expr
fromRat r = fromIntegral (numerator r) / fromIntegral (denominator r)

-------------------------------------------------------
-- Transformations

plusT, minusT, timesT, divisionT :: Fractional a => a -> Equation a -> Equation a
plusT     e = fmap (+e)
minusT    e = fmap (\x -> x-e)
timesT    e = fmap (e*) -- order is somehow significant
divisionT e = fmap (/e)

distributionT :: Expr -> Maybe Expr
distributionT (a :*: b) | length bs > 1 = do
   ratioView a -- restriction
   return $ foldr1 (+) (map (a*) bs)
 where bs = sumView b
distributionT (a :*: b) | length as > 1 = do
   ratioView a -- restriction
   return $ foldr1 (+) (map (b*) as)
   
 where as = sumView a
distributionT _ = Nothing

mergeTerms :: Equation Expr -> Equation Expr
mergeTerms = fmap (cleanup . merge)
 where
   make x [] = 0
   make x ((a, n):rest) = let (xs, ys) = partition ((==n) . snd) rest
                          in cleanup (foldr (+) a (map fst xs)) * (x^Con n) + make x ys
   
   merge e =
      case mapM powerView (sumView e) of
         Just es | length (nub xs) == 1 -> make (variable $ head xs) (zip as ns)
          where
            (as, xs, ns) = unzip3 es
         _ -> e

-------------------------------------------------------
-- Rewrite Rules

varToLeft :: Rule (Equation Expr)
varToLeft = makeSimpleRule "varToLeft" $ \eq -> do
   (a, x, _) <- linearView (getRHS eq)
   when (a==0) Nothing
   return $ mergeTerms $
      if (a > 0) then minusT (fromRat a       * variable x) eq
                 else plusT  (fromRat (abs a) * variable x) eq

conToRight :: Rule (Equation Expr)
conToRight = makeSimpleRule "conToRight" $ \eq -> do
   (_, _, b) <- linearView (getLHS eq)
   when (b==0) Nothing
   return $ mergeTerms $
      if (b > 0) then minusT (fromRat b) eq
                 else plusT  (fromRat (abs b)) eq

scaleToOne :: Rule (Equation Expr)
scaleToOne = makeSimpleRule "scaleToOne" $ \eq -> do
   (a, _, _) <- linearView (getLHS eq)
   when (a==0 || a==1) Nothing
   return $ mergeTerms $
      divisionT (fromRat a) eq

-- be more clever here, it is not always a good idea to get rid of the fraction
noDivision :: Rule (Equation Expr)
noDivision = makeSimpleRule "noDivision" $ \eq@(lhs :==: rhs) -> 
   case catMaybes (map f (sumView lhs ++ sumView rhs)) of
      b:_ -> return $ mergeTerms (timesT b eq)
      _   -> Nothing
 where
   f (Negate a) = f a
   f (a :/: b) = do
      constantView b
      return b
   f _ = Nothing

merge :: Rule (Equation Expr)
merge = makeSimpleRule "merge" $ \e -> 
   do let new = mergeTerms e
      unless (e /= new) Nothing
      return new
      
distribute :: Rule (Equation Expr)
distribute = makeSimpleRule "distribute" $ \(lhs :==: rhs) -> 
   let f = somewhereM distributionT in
   case (f lhs,f rhs) of
      (Just new, _) -> Just (cleanup new :==: rhs)
      (_, Just new) -> Just (lhs :==: cleanup new)
      _  -> Nothing

linStrat :: Strategy (Equation Expr)
linStrat =  repeat (noDivision |> merge |> distribute) 
        <*> try varToLeft 
        <*> try conToRight 
        <*> try scaleToOne
 
----------------------------------------------------------------------
-- Linear Equations Exercise Sets (from DWO environment)

gaan = traceStrategy linStrat (level5 !! 9)

testAll = putStrLn $ unlines $ map show $ map (runStrategy linStrat) (concat levels)

main :: IO ()
main = flip mapM_ [ (level, i) | level <- [1..5], i <- [1..10] ] $ \(level, i) -> do
   let line  = putStrLn (replicate 50 '-')  
       start = (levels !! (level-1)) !! (i-1)
   line
   putStrLn $ "Exercise " ++ show i ++ " (level " ++ show level ++ ")"
   line 
   case derivations linStrat start of
      hd:_ -> showDerivation "" hd
      _    -> putStrLn "unsolved"

levels :: [[Equation Expr]]
levels = [level1, level2, level3, level4, level5]

level1 :: [Equation Expr]
level1 = 
   let x = variable "x" in
   [ 5*x + 3   :==: 18
   , 11*x - 12 :==: 21
   , 19 - 3*x  :==: -5
   , -12 + 5*x :==: 33
   , 15 - 9*x  :==: 6
   , 4*x + 18  :==: 0
   , 11*x - 12 :==: -34
   , -2*x - 3  :==: -4
   , 6*x - 12  :==: 2
   , -4*x - 13 :==: -11
   ]

level2 :: [Equation Expr]
level2 = 
   let x = variable "x" in
   [ 6*x-2    :==: 2*x+14
   , 3+6*x    :==: 3*x+24
   , 5*x+7    :==: 2*x - 10
   , 2*x-8    :==: 18 - x
   , 4*x - 6  :==: 7*x - 14
   , -1 -5*x  :==: 3*x - 20
   , 4*x - 7  :==: -5*x - 24
   , 4*x - 18 :==: 14 + 11*x
   , 17       :==: 4 - 10*x
   , -5*x + 6 :==: 2 - 3*x
   ]

level3 :: [Equation Expr]
level3 = 
   let x = variable "x" in
   [ 4*(x-1)          :==: 11*x - 12
   , 4*(x-4)          :==: 5*(2*x+1)
   , 2*(5-3*x)        :==: 6-x
   , 4*x - (x-2)      :==: 12 + 5*(x-1)
   , -3*(x-2)         :==: 3*(x+4) - 7
   , 3*(4*x-1) + 3    :==: 7*x - 14
   , 4*(4*x - 1) - 2  :==: -3*x + 3*(2*x -5)
   , 2*x - (3*x + 5)  :==: 10 + 5*(x-1)
   , -5*(x+1)         :==: 9*(x+4)-5
   , 18 - 2*(4*x + 2) :==: 7*x - 4*(4*x -2)
   ]

level4 :: [Equation Expr]
level4 = 
   let x = variable "x" in
   [ (1/2)*x - 4            :==: 2*x + 2+(1/2)
   , (1/4)*x + (1/2)        :==: (5/2)*x + 2
   , (1/4)*x - (3/4)        :==: 2*x + (1/2)
   , -(1/2)*x + (3/4)       :==: (5/2)*x + 3
   , -(1/2)*x + 1+(1/2)     :==: 2*x - 5
   , -(1/3)*x + (3/4)       :==: (1/4)*x + (1/6)
   , (3/4)*x - (1/3)        :==: (2/3)*x - (3/4)
   , (2/5)*x - (1/4)        :==: (1/2)*x + (3/4)
   , (2/3)*x - 2            :==: (1/5)*x - (3/5)
   , (-1+(2/5))*x + 3+(1/2) :==: (3/5)*x + (9/10)
   ]

level5 :: [Equation Expr]
level5 = 
   let x = variable "x" in
   [ (1/4)*(x-3)         :==: (1/2)*x - 4
   , (x+3)/2             :==: 5*((1/2)*x + 1 + (1/2))
   , (1/2)*(7-(2/3)*x)   :==: 2 + (1/9)*x
   , (3/4)*x - (x-1)     :==: 3 + (2+(1/2))*(x-1)
   , -(5/4)*(x-7)        :==: (3/4)*(x+2) - (4+(1/2))
   , 3*((1/5)*x - 1) + 5 :==: 7*x - 14
   , ((5*x - 1) / 6) - 2 :==: -4*x + (3*x - 6)/2
   , 2*x - ((2*x+2)/5)   :==: 12 + (x-1)/6
   , (-3*(x+2))/6        :==: 9*((2/3)*x + (1/3)) - (5/3)
   , 1 - ((4*x + 2)/3)   :==: 3*x - ((5*x - 1) / 4)
   ]