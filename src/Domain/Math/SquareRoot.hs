module Domain.Math.SquareRoot 
   ( SquareRoot, imaginary, imaginaryUnit, con, toList, scale
   , sqrt, sqrtRational, isqrt
   , safeDiv, safeRecip
   ) where

import Prelude hiding (sqrt)
import Common.Utils (primes)
import Data.Ratio
import qualified Data.IntMap as IM
import qualified Prelude

-- Sum of square roots (possibly imaginary) that are normalized 
--
-- Invariants: 
-- * all keys are normalized (sqrt 8 -> 2*(sqrt 2))
-- * all values are non-zero
-- * We maintain the "imaginary" property since sqrt(-1)*sqrt(-1) may or may not
--   be equal to sqrt(1)
data SquareRoot a = S 
   { imaginary     :: Bool
   , squareRootMap :: IM.IntMap a
   } deriving (Show, Eq, Ord)

-- the Functor instance does not maintain the invariant (non-zero)
instance Functor SquareRoot where
   fmap f (S b m) = S b (IM.map f m)

instance Num a => Num (SquareRoot a) where
   negate (S b m)    = S b (fmap negate m)
   fromInteger       = con . fromInteger
   S b1 m1 + S b2 m2 = S (b1 || b2) (IM.filter (/=0) (IM.unionWith (+) m1 m2))
   s       * S b2 m2 = 
      let op n a m = timesSqrt n (scale a s) + m
          start    = if b2 then 0 {imaginary = True} else 0
      in  IM.foldWithKey op start m2
   
   -- not defined for square roots
   abs    = error "abs not defined for square roots"
   signum = error "signum not defined for square roots"

safeDiv :: Fractional a => SquareRoot a -> SquareRoot a -> Maybe (SquareRoot a)
safeDiv a b = fmap (a*) (safeRecip b)

safeRecip :: Fractional a => SquareRoot a -> Maybe (SquareRoot a)
safeRecip a =
   case toList a of 
      [(x, n)] -> Just (con (recip (x * fromIntegral n)) * sqrt (fromIntegral n))
      _        -> Nothing

------------------------------------------------

imaginaryUnit :: Num a => SquareRoot a
imaginaryUnit = S True (IM.singleton (-1) 1)

toList :: SquareRoot a -> [(a, Int)]
toList = map (\(k, r) -> (r, k)) . IM.toList . squareRootMap

con :: Num a => a -> SquareRoot a
con a = S False (if a==0 then IM.empty else IM.singleton 1 a)

sqrt :: Num a => Integer -> SquareRoot a
sqrt n =
   case compare n 0 of
      EQ -> 0
      LT -> imaginaryUnit * helper (abs n)
      GT -> helper n

 where
   -- helper gets a positive number
   -- primesSquared !! 50 equals 54289
   helper :: Num a => Integer -> SquareRoot a
   helper n 
      | a*a == n  = fromIntegral a 
      | otherwise = f 1 (fromIntegral n) (take 50 $ zip primes primesSquared)
    where
      a = isqrt n
  
      f :: Num a => Int -> Int -> [(Int, Int)] -> SquareRoot a
      f acc n xs@((i,isq):rest)
         | m == 0  = f (i*acc) d xs
         | n > isq = f acc n rest
       where 
         (d, m) = n `divMod` isq
      f acc n _ = S False (IM.singleton (fromIntegral n) (fromIntegral acc))

scale :: Num a => a -> SquareRoot a -> SquareRoot a
scale a sr = if a==0 then 0 else fmap (*a) sr
               
------------------------------------------------

isqrt :: Integer -> Integer
isqrt = floor . Prelude.sqrt . fromInteger

primesSquared :: [Int]
primesSquared = map (\x -> x*x) primes

timesSqrt :: Num a => Int -> SquareRoot a -> SquareRoot a
timesSqrt i (S b m) = S (b || i < 0) $ 
   case compare i 0 of 
      EQ -> make (const [])
      LT -> make reverse
      GT -> make id
 where
   make f = IM.fromAscList $ f [ (n*i, a) | (n, a) <- IM.toList m ]

sqrtRational :: Fractional a => Rational -> SquareRoot a
sqrtRational r = scale (1/fromIntegral (denominator r)) (sqrt (numerator r * denominator r))