-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.Vector 
   ( Vector, fromList, toList, liftV, liftV2, showVectorWith
   , toUnit, isUnit, makeOrthogonal, orthogonal, orthonormalList
   , scale, norm, distance, vectorSum, innerProduct
   ) where

import Data.List

newtype Vector a = Vector [a]
   deriving (Eq, Ord)

instance Functor Vector where
   fmap f (Vector xs) = Vector (map f xs)

instance Show a => Show (Vector a) where
   show = showVectorWith show

instance Num a => Num (Vector a) where
   (+) = liftV2 (+)
   (*) = liftV2 (*)
   (-) = liftV2 (-)
   negate = liftV negate
   abs    = liftV abs
   signum = liftV signum
   fromInteger = fromList . return . fromInteger

showVectorWith :: (a -> String) -> Vector a -> String
showVectorWith f (Vector xs) = "(" ++ concat (intersperse "," (map f xs)) ++ ")"

toList :: Vector a -> [a]
toList (Vector xs) = xs

fromList :: [a] -> Vector a
fromList = Vector

-- local helper function
liftV :: (a -> b) -> Vector a -> Vector b
liftV op = fromList . map op . toList

-- local helper function
liftV2 :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
liftV2 op v1 v2 = fromList $ zipWith op (toList v1) (toList v2)

 

toUnit :: Floating a => Vector a -> Vector a
toUnit v = scale (1 / norm v) v

isUnit :: Floating a => Vector a -> Bool
isUnit v = norm v == 1

makeOrthogonal :: Num a => Vector a -> Vector a -> Vector a
makeOrthogonal v1 v2 = v2 - scale (innerProduct v1 v2) v1

orthogonal :: Num a => Vector a -> Vector a -> Bool
orthogonal v1 v2 = innerProduct v1 v2 == 0

scale :: Num a => a -> Vector a -> Vector a
scale a = liftV (*a)

orthonormalList :: Floating a => [Vector a] -> Bool
orthonormalList xs = all isUnit xs && all (uncurry orthogonal) pairs
 where
   pairs = [ (a, b) | (i, a) <- zip [0..] xs, (j, b) <- zip [0..] xs, i < j ] 

-- length of the vector (also called norm)
norm :: Floating a => Vector a -> a
norm v = sqrt $ innerProduct v v
 
distance :: Floating a => Vector a -> Vector a -> a
distance v1 v2 = norm (v1 - v2)
 
vectorSum :: Num a => Vector a -> a
vectorSum = sum . toList
 
innerProduct :: Num a => Vector a -> Vector a -> a
innerProduct v1 v2 = vectorSum (v1 * v2)