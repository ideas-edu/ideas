module Matrix.LinearExpr 
   ( LinearExpr, var, isVar, getVars, isConstant, getConstant, renameVariables
   , substVar, coefficientOf, toLinearExpr, splitLinearExpr, evalLinearExpr, scaleLinearExpr
   ) where

import Utils
import Data.List
import qualified Data.Map as M
import GHC.Real
import Test.QuickCheck
import Control.Monad

-- invariant: coefficients are /= 0
data LinearExpr a = LE (M.Map String a) a
   deriving Eq

-- Smart constructor that establishes the invariant
linearExpr :: Num a => M.Map String a -> a -> LinearExpr a
linearExpr = LE . M.filter (/=0)

instance Num a => Show (LinearExpr a) where
   show (LE xs c)
      | M.null xs = show c
      | otherwise  =  
           let f (a, b) = if b==1 then a else show b ++ "*" ++ a
           in concat $ intersperse " + " $ map f (M.toList xs) ++ [show c | c /= 0 ]

instance Num a => Num (LinearExpr a) where
   LE xs a + LE ys b = linearExpr (M.unionWith (+) xs ys) (a+b)
   x - y = x + negate y
   x * y = case (isConstant x, isConstant y) of
              (Just n, _) -> scaleLinearExpr n y
              (_, Just n) -> scaleLinearExpr n x
              _           -> error "LinExp.(*): resulting expression is not linear" 
   negate      = scaleLinearExpr (-1)
   fromInteger = toLinearExpr . fromIntegral

instance Fractional a => Fractional (LinearExpr a) where
   fromRational (n :% m) = toLinearExpr (fromInteger n / fromInteger m)
   x / y = case isConstant y of  
              Just n  -> x * toLinearExpr (1/n)
              Nothing -> error "LinExp.(/): resulting expression is not linear"

-- Attention: invariant cannot be checked at this point. The function 'f' should
-- not introduce zero coefficients
instance Functor LinearExpr where
   fmap f (LE xs a) = LE (M.map f xs) (f a)
      
scaleLinearExpr :: Num a => a -> LinearExpr a -> LinearExpr a
scaleLinearExpr a (LE xs c) = linearExpr (M.map (*a) xs) (c*a)

var :: Num a => String -> LinearExpr a
var x = LE (M.singleton x 1) 0

getVars :: LinearExpr a -> [String]
getVars (LE xs _) = M.keys xs

getConstant :: LinearExpr a -> a
getConstant (LE _ c) = c

toLinearExpr :: Num a => a -> LinearExpr a
toLinearExpr = LE M.empty

splitLinearExpr :: Num a => (String -> Bool) -> LinearExpr a -> (LinearExpr a, LinearExpr a)
splitLinearExpr p (LE xs c) = (LE ys c, LE zs 0)
 where (ys, zs) = M.partitionWithKey (\k _ -> p k) xs

-- substitution
substVar :: Num a => String -> LinearExpr a -> LinearExpr a -> LinearExpr a 
substVar var a e@(LE xs c) =
   toLinearExpr (coefficientOf var e) * a + LE (M.delete var xs) c

coefficientOf :: Num a => String -> LinearExpr a  -> a
coefficientOf s (LE xs b) = M.findWithDefault 0 s xs

isVar :: Num a => LinearExpr a -> Maybe String
isVar e = case getVars e of 
             [v] | getConstant e == 0 && coefficientOf v e == 1
                -> return v
             _  -> Nothing

isConstant :: LinearExpr a -> Maybe a
isConstant e
   | null (getVars e) = return (getConstant e)
   | otherwise        = Nothing

evalLinearExpr :: Num a => (String -> a) -> LinearExpr a -> a
evalLinearExpr f (LE xs a) =
   M.foldWithKey (\s b c -> f s * b + c) a xs

renameVariables :: (String -> String) -> LinearExpr a -> LinearExpr a
renameVariables f (LE xs a) = LE (M.mapKeys f xs) a

-----------------------------------------------------
-- QuickCheck generators

instance (Num a, Arbitrary a) => Arbitrary (LinearExpr a) where
   arbitrary = arbLinExp (map (\n -> 'x' : show n) [1..5])
   coarbitrary (LE xs a) = coarbitrary xs . coarbitrary a

arbLinExp :: (Num a, Arbitrary a) => [String] -> Gen (LinearExpr a)
arbLinExp vars = liftM2 linearExpr gen arbitrary
 where 
   len = length vars
   gen = liftM2 merge (vector len) (vector len)
   merge bs as = M.fromList [ (v, a) | (v, a, b) <- zip3 vars as bs, b ]