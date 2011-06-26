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
module Domain.Math.Data.Polynomial 
   ( Polynomial, var, con, raise
   , degree, lowestDegree, coefficient, terms
   , isMonic, toMonic, isRoot, positiveRoots, negativeRoots
   , derivative, eval, longDivision, polynomialGCD
   , factorize
   ) where

import Common.Classes
import Control.Applicative (Applicative, (<$>), liftA)
import Control.Monad
import Data.Char
import Data.Foldable (Foldable, foldMap)
import Data.List  (nub)
import Data.Ratio (approxRational)
import Data.Traversable (Traversable, sequenceA)
import Domain.Math.Approximation (newton, within)
import Domain.Math.Safe
import Test.QuickCheck hiding (within)
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
      | degree p1 < degree p2 = Nothing
      | b==0      = return a
      | otherwise = Nothing 
    where 
      (a, b) = longDivision p1 p2

-- the Functor instance does not maintain the invariant
instance Functor Polynomial where
   fmap f = P . IM.map f . unsafeP

instance Foldable Polynomial where
   foldMap f = foldMap f . unsafeP
   
instance Traversable Polynomial where
   sequenceA = liftA P . sequenceIntMap . unsafeP

instance Num a => Num (Polynomial a) where
   p1 + p2 = makeP $ IM.unionWith (+) (unP p1) (unP p2)
   p1 * p2 = makeP $ foldr (uncurry op) IM.empty list
    where
      op   = IM.insertWith (+)
      list = [ (i+j, a*b) | (a, i) <- terms p1, (b, j) <- terms p2 ]
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

terms :: Num a => Polynomial a -> [(a, Int)]
terms p = [ (a, n) | (n, a) <- IM.toList (unP p) ]

isMonic :: Num a => Polynomial a -> Bool
isMonic p = coefficient (degree p) p == 1

toMonic :: Fractional a => Polynomial a -> Polynomial a
toMonic p = con (recip a) * p
 where a = coefficient (degree p) p

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
longDivision :: Fractional a => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
longDivision p1 p2 = monicLongDivision (f p1) (f p2)
 where 
   f p = con (recip a) * p
   a   = coefficient (degree p2) p2

-- polynomial long division, where p2 is monic
monicLongDivision :: Num a => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
monicLongDivision p1 p2
   | d1 >= d2 && isMonic p2 = (toP quotient, toP remainder)
   | otherwise = error $ "invalid monic division" ++ show (p1, p2)
 where
   d1 = degree p1
   d2 = degree p2
   xs = map (`coefficient` p1) [d1, d1-1 .. 0]
   ys = drop 1 $ map (negate . (`coefficient` p2)) [d2, d2-1 .. 0]
   
   (quotient, remainder) = rec [] xs
   toP = makeP . IM.fromAscList . zip [0..]
   
   rec acc (a:as) | length as >= length ys = 
      rec (a:acc) (zipWith (+) (map (*a) ys ++ repeat 0) as)
   rec acc as = (acc, reverse as)
   
-- use polynomial long division to compute the greatest common factor 
-- of the polynomials
polynomialGCD :: Fractional a => Polynomial a -> Polynomial a -> Polynomial a
polynomialGCD x y
   | degree y > degree x = rec y x 
   | otherwise           = rec x y
 where
   rec a b
      | b == 0    = a 
      | otherwise = rec b (snd (longDivision a b))
   
------------------------

factorize :: Polynomial Rational -> [Polynomial Rational]
factorize p
   | degree p <= 1 = [p]
   | l > 0         = var ^ l : factorize (raise (-l) p)
   | otherwise     =
        case pairs of
           (p1,p2):_ -> factorize p1 ++ factorize p2
           []        -> [p]
 where
   l     = snd (head (terms p))
   pairs = [ (p1, p2) 
           | a <- candidateRoots p
           , isRoot p a 
           , let p1 = var - con a
           , Just p2 <- [safeDiv p p1]
           ] 
           
candidateRoots :: Polynomial Rational -> [Rational]
candidateRoots p = nub (map (`approxRational` 0.0001) xs)
 where
    f  = eval (fmap fromRational p)
    df = eval (fmap fromRational (derivative p))
    xs = nub (map (within 0.0001 . take 10 . newton f df) startList)
    startList = [0, 3, -3, 10, -10, 100, -100]
    
-- TODO: replace me by sequenceA
-- This definition is for backwards compatibility. In older versions of IntMap, 
-- the instance for Traversable is lacking. 
sequenceIntMap :: Applicative m => IM.IntMap (m a) -> m (IM.IntMap a)
sequenceIntMap m = IM.fromDistinctAscList <$> zip ks <$> sequenceA as
 where
   (ks, as) = unzip (IM.toList m)