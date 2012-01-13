-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.Data.Polynomial
   ( Polynomial, toPolynomial, fromPolynomial, var, con, raise
   , degree, lowestDegree, coefficient
   , isRoot, positiveRoots, negativeRoots
   , derivative, eval, polynomialGCD, factorize
   , testPolynomials
   ) where

import Common.Classes
import Control.Applicative (Applicative, (<$>), liftA)
import Common.Utils.TestSuite
import Control.Monad
import Data.Char
import Data.Foldable (Foldable, foldMap)
import Data.Ratio
import Data.Traversable (Traversable, sequenceA)
import Domain.Math.Safe
import Domain.Math.Data.Primes
import Test.QuickCheck
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

------------------------------------------------------------------
-- Data type:
--   Invariant: all keys are non-negative, all values are non-zero
--   (note that the second part of the invariant (zero values)
--    can be violated using the functor instance)

newtype Polynomial a = P { unsafeP :: IM.IntMap a }

invariant :: Num a => IM.IntMap a -> IM.IntMap a
invariant = IM.filterWithKey (\n a -> n >= 0 && a /= 0)

makeP :: Num a => IM.IntMap a -> Polynomial a
makeP = P . invariant

unP :: Num a => Polynomial a -> IM.IntMap a
unP = invariant . unsafeP

toPolynomial :: Num a => [a] -> Polynomial a
toPolynomial = makeP . IM.fromAscList . zip [0..] . reverse

fromPolynomial :: Num a => Polynomial a -> [a]
fromPolynomial p = map (`coefficient` p) [d, d-1 .. 0]
 where d = degree p 

-------------------------------------------------------------------
-- Instances

instance Num a => Eq (Polynomial a) where
   p1 == p2 = unP p1 == unP p2

instance Num a => Show (Polynomial a) where
   show p
      | p ==0     = "f(x) = 0"
      | otherwise = "f(x) = " ++ fix (concatMap f (reverse (IM.toList (unP p))))
    where
      f (n, a) = sign (one (show a ++ g n))
      g n = concat $ [ "x" | n > 0 ] ++ [ '^' : show n | n > 1 ]
      one ('1':xs@('x':_))     = xs
      one ('-':'1':xs@('x':_)) = xs
      one xs                   = xs
      sign ('-':xs) = " - " ++ xs
      sign xs       = " + " ++ xs
      fix xs = case dropWhile isSpace xs of
                  '+':ys -> dropWhile isSpace ys
                  '-':ys -> '-':dropWhile isSpace ys
                  ys     -> ys

instance Fractional a => SafeDiv (Polynomial a) where
   -- polynomial division, no remainder
   safeDiv p1 p2
      | p2==0     = Nothing
      | degree p1 < degree p2 = Nothing
      | b==0      = return a
      | otherwise = Nothing
    where
      (a, b) = divModPoly p1 p2

-- the Functor instance does not maintain the invariant
instance Functor Polynomial where
   fmap f = P . IM.map f . unsafeP

instance Foldable Polynomial where
   foldMap f = foldMap f . unsafeP

instance Traversable Polynomial where
   sequenceA = liftA P . sequenceIntMap . unsafeP

instance Num a => Num (Polynomial a) where
   p1 + p2 = makeP $ IM.unionWith (+) (unP p1) (unP p2)
   p1 * p2 = sum [ raise i (fmap (*a) p1) | (i, a) <- IM.toList (unP p2) ]
   
   {- makeP $ foldr (uncurry op) IM.empty list
    where
      op   = IM.insertWith (+)
      list = [ (i+j, a*b) | (a, i) <- terms p1, (b, j) <- terms p2 ] -}
   negate      = fmap negate
   fromInteger = makeP . IM.singleton 0 . fromInteger
   -- not defined for polynomials
   abs    = error "abs not defined for polynomials"
   signum = error "signum not defined for polynomials"

instance (Arbitrary a, Num a) => Arbitrary (Polynomial a) where
   arbitrary = do
      d <- choose (0, 5)
      let f n x = con x * var ^ n
      liftM (sum . zipWith f [0::Int ..]) (vector (d+1))

-------------------------------------------------------------------
-- Functions on polynomials

-- a single variable (such as "x")
var :: Num a => Polynomial a
var = makeP (IM.singleton 1 1)

con :: Num a => a -> Polynomial a
con = makeP . IM.singleton 0

-- | Raise all powers by a constant (discarding negative exponents)
raise :: Num a => Int -> Polynomial a -> Polynomial a
raise i = makeP . IM.fromAscList . map (mapFirst (+i)) . IM.toList . unP

------------------------------------------------

degree :: Num a => Polynomial a -> Int
degree p
   | IS.null is = 0
   | otherwise  = IS.findMax is
 where is = IM.keysSet (unP p)

lowestDegree :: Num a => Polynomial a -> Int
lowestDegree p
   | IS.null is = 0
   | otherwise  = IS.findMin is
 where is = IM.keysSet (unP p)

coefficient :: Num a => Int -> Polynomial a -> a
coefficient n = IM.findWithDefault 0 n . unP

isRoot :: Num a => Polynomial a -> a -> Bool
isRoot p a = eval p a == 0

-- Returns the maximal number of positive roots (Descartes theorem)
-- Multiple roots are counted separately
positiveRoots :: Num a => Polynomial a -> Int
positiveRoots = signChanges . IM.elems . unP

-- Returns the maximal number of negative roots (Descartes theorem)
-- Multiple roots are counted separately
negativeRoots :: Num a => Polynomial a -> Int
negativeRoots = signChanges . flipOdd . IM.elems . unP
 where
   flipOdd (x:y:zs) = x:negate y:flipOdd zs
   flipOdd xs = xs

signChanges :: Num a => [a] -> Int
signChanges = f . map signum
 where
   f (x:xs@(hd:_)) = if x==hd then f xs else 1 + f xs
   f _ = 0

------------------------------------------------

derivative :: Num a => Polynomial a -> Polynomial a
derivative p = makeP $ IM.fromAscList
   [ (n-1, fromIntegral n*a) | (n, a) <- IM.toList (unP p) ]

eval :: Num a => Polynomial a -> a -> a
eval p x = sum [ a * x^n | (n, a) <- IM.toList (unP p) ]

-- polynomial long division
divModPoly :: Fractional a => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
divModPoly p1 p2 = mapBoth toPolynomial $ 
   longDivision (fromPolynomial p2) (fromPolynomial p1)

-- use polynomial long division to compute the greatest common factor
-- of the polynomials
polynomialGCD :: Fractional a => Polynomial a -> Polynomial a -> Polynomial a
polynomialGCD x y
   | degree y > degree x = rec y x
   | otherwise           = rec x y
 where
   rec a b
      | b == 0    = a
      | otherwise = rec b (snd (divModPoly a b))

------------------------

factorize :: Polynomial Rational -> [Polynomial Rational]
factorize = map toPolynomial . make . fromPolynomial
 where
   make ps 
      | null ps      = [[]]
      | head ps == 0 = make (tail ps)
      | last ps == 0 = [1, 0] : make (init ps)
      | otherwise    = rec ps $ possibleRoots (last is) (head is)
    where
      is = toInts ps
        
   rec ps [] = [ ps | ps /= [1] ]
   rec ps list@(r:rs) 
      | b == 0     = [1, -r] : rec qs list
      | otherwise  = rec ps rs
    where
      (qs, b) = syntheticDivision r ps

toInts :: [Rational] -> [Int]
toInts ps = map (`div` a) is
 where
   is  = map f ps
   d   = foldr1 lcm (map denominator ps)
   f x = fromIntegral $ (numerator x * d) `div` denominator x
   a   = foldr1 gcd is
      
possibleRoots :: Int -> Int -> [Rational]
possibleRoots a b = reverse (map negate xs) ++ xs
 where
   xs  = map f (factors (abs a)) -- or: factors (abs (a*b))
   f x = toRational x / toRational b

-- TODO: replace me by sequenceA
-- This definition is for backwards compatibility. In older versions of IntMap,
-- the instance for Traversable is lacking.
sequenceIntMap :: Applicative m => IM.IntMap (m a) -> m (IM.IntMap a)
sequenceIntMap m = IM.fromDistinctAscList <$> zip ks <$> sequenceA as
 where
   (ks, as) = unzip (IM.toList m)

---------------------------------------------------------------
-- Algorithms for synthetic and long division

{- syntheticDivision a p: divide polynomial p by (x-a)
   Example:
   
      -3|  1   7    11  -3
              -3   -12   3
      -------------------- +
           1   4    -1   0   (last number is remainder)
   -}
syntheticDivision :: Num a => a -> [a] -> ([a], a)
syntheticDivision a xs = (init zs, last zs)
 where
   ys = 0 : map (*a) zs
   zs = zipWith (+) xs ys

{- longDivision p q: divide polynomial q by p
   Example:
   
      x+3|   1   10   24
             1    3          (1x)
             ----------- - 
                  7   24     (7x)
                  7   21 
                  ------ -
                       3    (remainder)
   -}
longDivision :: Fractional a => [a] -> [a] -> ([a], [a])
longDivision []     = error "longDivision by zero"
longDivision (0:xs) = longDivision xs
longDivision (x:xs) = recN
 where
   recN ys = rec (length ys - length xs) ys
   
   rec n (y:ys) | n > 0 =
      let d  = y/x
          zs = zipWith (-) ys (map (*d) xs ++ repeat 0)
      in mapFirst (d:) (rec (n-1) zs)
   rec _ ys = ([], ys)

---------------------------------------------------------------
-- Properties

testPolynomials :: TestSuite
testPolynomials = suite "polynomial" $ 
   addProperty "factorization" $ do
      i  <- choose (0, 5)
      as <- replicateM i $ choose (-20, 20)
      b  <- choose (1, 30) 
      c  <- choose (-10*b, 10*b)
      let qs = [ var - con (fromInteger a) | a <- as ]
          p  = con (fromInteger c/fromInteger b) * product qs
          ps = factorize p
      return (all ((<= 1) . degree) ps && product ps == p)