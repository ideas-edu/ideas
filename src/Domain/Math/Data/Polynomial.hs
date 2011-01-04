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
   ( Polynomial, var, con, raise, power, scale
   , degree, lowestDegree, coefficient, terms
   , isMonic, toMonic, isRoot, positiveRoots, negativeRoots
   , derivative, eval, division, longDivision, polynomialGCD
   , factorize
   ) where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Char
import Data.List  (nub)
import Data.Foldable (Foldable, foldMap)
import Data.Traversable (Traversable, sequenceA)
import Control.Applicative (Applicative, (<$>))
import Data.Ratio (approxRational)
import Domain.Math.Approximation (newton, within)

-- Invariants: all keys are non-negative, all values are non-zero
newtype Polynomial a = P (IM.IntMap a) deriving Eq

instance Num a => Show (Polynomial a) where
   show (P m) 
      | IM.null m = "f(x) = 0"
      | otherwise = "f(x) = " ++ fix (concatMap f (reverse (IM.toList m)))
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

-- the Functor instance does not maintain the invariant
instance Functor Polynomial where
   fmap f (P m) = P (IM.map f m)

instance Foldable Polynomial where
   foldMap f (P m) = foldMap f m
   
instance Traversable Polynomial where
   sequenceA (P m) = P <$> sequenceIntMap m

instance Num a => Num (Polynomial a) where
   P m1 + P m2   = P (IM.filter (/= 0) (IM.unionWith (+) m1 m2))
   p    * P m2   = IM.foldWithKey op 0 m2
    where op n a m = raise n (scale a p) + m
   negate         = fmap negate
   fromInteger n
      | n == 0    = P IM.empty
      | otherwise = P (IM.singleton 0 (fromInteger n))
   -- not defined for polynomials
   abs    = error "abs not defined for polynomials"
   signum = error "signum not defined for polynomials"

-- a single variable (such as "x") 
var :: Num a => Polynomial a
var = P (IM.singleton 1 1)

con :: a -> Polynomial a
con = P . IM.singleton 0

-- | Raise all powers by a constant (discarding negative exponents)
raise :: Int -> Polynomial a -> Polynomial a
raise i p@(P m)
   | i > 0     = P $ IM.fromAscList [ (n+i, a) | (n, a) <- IM.toList m ]
   | i == 0    = p
   | otherwise = P $ IM.fromAscList [ (n+i, a) | (n, a) <- IM.toList m, n+i>=0 ]
 
power :: Num a => Polynomial a -> Int -> Polynomial a
power _ 0 = 1
power p n = p * power p (n-1)

scale :: Num a => a -> Polynomial a -> Polynomial a
scale a p = if a==0 then 0 else fmap (*a) p

------------------------------------------------

degree :: Polynomial a -> Int
degree (P m)
   | IS.null is = 0
   | otherwise  = IS.findMax is
 where is = IM.keysSet m

lowestDegree :: Polynomial a -> Int
lowestDegree (P m)
   | IS.null is = 0
   | otherwise  = IS.findMin is
 where is = IM.keysSet m

coefficient :: Num a => Int -> Polynomial a -> a
coefficient n (P m) = IM.findWithDefault 0 n m

terms :: Polynomial a -> [(a, Int)]
terms (P m) = [ (a, n) | (n, a) <- IM.toList m ]

isMonic :: Num a => Polynomial a -> Bool
isMonic p = coefficient (degree p) p == 1

toMonic :: Fractional a => Polynomial a -> Polynomial a
toMonic p = scale (recip a) p
 where a = coefficient (degree p) p

isRoot :: Num a => Polynomial a -> a -> Bool
isRoot p a = eval p a == 0

-- Returns the maximal number of positive roots (Descartes theorem)
-- Multiple roots are counted separately
positiveRoots :: Num a => Polynomial a -> Int
positiveRoots (P m) = signChanges (IM.elems m)

-- Returns the maximal number of negative roots (Descartes theorem)
-- Multiple roots are counted separately
negativeRoots :: Num a => Polynomial a -> Int
negativeRoots (P m) = signChanges (flipOdd (IM.elems m))
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
derivative (P m) = P $ IM.fromAscList 
   [ (n-1, fromIntegral n*a) | (n, a) <- IM.toList m, n > 0 ]

eval :: Num a => Polynomial a -> a -> a
eval (P m) x = sum [ a * x^n | (n, a) <- IM.toList m ] 

-- polynomial division, no remainder
division :: Fractional a => Polynomial a -> Polynomial a -> Maybe (Polynomial a)
division p1 p2
   | degree p1 < degree p2 = Nothing
   | b==0      = return a
   | otherwise = Nothing 
 where 
   (a, b) = longDivision p1 p2

-- polynomial long division
longDivision :: Fractional a => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
longDivision p1 p2 = monicLongDivision (scale (recip a) p1) (scale (recip a) p2)
 where a = coefficient (degree p2) p2

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
   toP = P . IM.filter (/= 0) . IM.fromAscList . zip [0..]
   
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
   | l > 0         = power var l : factorize (raise (-l) p)
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
           , Just p2 <- [division p p1]
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