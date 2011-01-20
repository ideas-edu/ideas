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
module Domain.LinearAlgebra.Matrix 
   ( Matrix, Row, Column, isRectangular, makeMatrix, identity, mapWithPos
   , changeEntries, changeEntry, setEntries, setEntry
   , rows, row, columns, column, dimensions, entry, isEmpty
   , add, scale, multiply
   , reduce, forward, backward, inverse, invertible, rank, nullity, eqMatrix
   , switchRows, scaleRow, addRow
   , inRowEchelonForm, inRowReducedEchelonForm
   , nonZero, pivot, isPivotColumn
   , isSquare, identityMatrix, isLowerTriangular, isUpperTriangular
   ) where

import Common.Rewriting
import Control.Monad
import Data.List hiding (transpose)
import Data.Maybe
import Data.Monoid
import Data.Foldable (Foldable, foldMap)
import Data.Traversable (Traversable, sequenceA)
import Control.Applicative
import Domain.Math.Simplification
import Test.QuickCheck
import qualified Text.OpenMath.Dictionary.Linalg2 as OM
import qualified Data.List as L
import qualified Data.Map as M

-- Invariant: a matrix is always rectangular
newtype Matrix a = M [[a]]
   deriving (Eq, Ord, Show)

type Row    a = [a]
type Column a = [a]

instance Functor Matrix where 
   fmap f (M rs) = M (map (map f) rs)

instance Foldable Matrix where
   foldMap f (M xss) = foldMap (mconcat . map f) xss

instance Traversable Matrix where
   sequenceA (M xss) = M <$> sequenceA (map sequenceA xss)

instance IsTerm a => IsTerm (Matrix a) where
   toTerm = 
      let f = function matrixrowSymbol . map toTerm
      in function matrixSymbol . map f . rows
   fromTerm a = do
      rs  <- isFunction matrixSymbol a
      xss <- mapM (isFunction matrixrowSymbol) rs
      yss <- mapM (mapM fromTerm) xss
      guard (isRectangular yss)
      return (makeMatrix yss)

instance Arbitrary a => Arbitrary (Matrix a) where
   arbitrary = do
      (i, j) <- arbitrary
      arbSizedMatrix (i `mod` 5, j `mod` 5)

instance CoArbitrary a => CoArbitrary (Matrix a) where
   coarbitrary = coarbitrary . rows

arbSizedMatrix :: Arbitrary a => (Int, Int) -> Gen (Matrix a)
arbSizedMatrix (i, j) = 
   do rs <- replicateM i (vector j)
      return (makeMatrix rs)

matrixSymbol, matrixrowSymbol :: Symbol
matrixSymbol    = newSymbol OM.matrixSymbol
matrixrowSymbol = newSymbol OM.matrixrowSymbol

instance Simplify a => Simplify (Matrix a) where
   simplifyWith opt = fmap (simplifyWith opt)

-- Check whether the table is rectangular
isRectangular :: [[a]] -> Bool
isRectangular xss =
   case map length xss of
      []   -> True
      n:ns -> all (==n) ns

-- Constructor function that checks whether the table is rectangular
makeMatrix :: [Row a] -> Matrix a
makeMatrix rs
   | null (concat rs) = M []
   | isRectangular rs = M rs
   | otherwise        = error "makeMatrix: not rectangular"

identity :: Num a => Int -> Matrix a
identity n = M $ map f [0..n-1]
 where f i = replicate i 0 ++ [1] ++ replicate (n-i-1) 0

isEmpty :: Matrix a -> Bool
isEmpty (M xs) = null xs

rows :: Matrix a -> [Row a]
rows (M rs) = rs

row :: Int -> Matrix a -> Row a
row n = (!!n) . rows

columns :: Matrix a -> [Column a]
columns = rows . transpose

column :: Int -> Matrix a -> Column a
column n = (!!n) . columns

dimensions :: Matrix a -> (Int, Int)
dimensions m = (length $ rows m, length $ columns m)

entry :: (Int, Int) -> Matrix a -> a
entry (i, j) m = row i m !! j

mapWithPos :: ((Int, Int) -> a -> b) -> Matrix a -> Matrix b
mapWithPos f (M rs) = M $ zipWith g [0..] rs
 where g y = zipWith (\x -> f (y, x)) [0..]

changeEntries :: M.Map (Int, Int) (a -> a) -> Matrix a -> Matrix a
changeEntries mp = mapWithPos (\pos -> M.findWithDefault id pos mp)

changeEntry :: (Int, Int) -> (a -> a) -> Matrix a -> Matrix a
changeEntry pos = changeEntries . M.singleton pos

setEntries :: M.Map (Int, Int) a -> Matrix a -> Matrix a
setEntries mp = mapWithPos (\pos a -> M.findWithDefault a pos mp)

setEntry :: (Int, Int) -> a -> Matrix a -> Matrix a
setEntry pos = setEntries . M.singleton pos

-------------------------------------------------------

add :: Num a => Matrix a -> Matrix a -> Matrix a
add a b
   | dimensions a == dimensions b =
        M $ zipWith (zipWith (+)) (rows a) (rows b)
   | otherwise =
        error "add: dimensions differ"

scale :: Num a => a -> Matrix a -> Matrix a
scale a = fmap (*a)

multiply :: Num a => Matrix a -> Matrix a -> Matrix a
multiply a b 
   | snd (dimensions a) == fst (dimensions b) =
        M $ map (\r -> map (sum . zipWith (*) r) (columns b)) (rows a)
   | otherwise =
        error "multiply: incorrect dimensions"

-------------------------------------------------------
-- Gaussian Elimination

reduce :: Fractional a => Matrix a -> Matrix a
reduce = backward . forward

forward :: Fractional a => Matrix a -> Matrix a
forward m 
   | h==0 || w==0 = m
   | all (==0) col = M $ zipWith (:) (repeat 0) $ rows $ forward $ M $ map tail $ rows m
   | x == 0 = forward (switchRows 0 (fromJust $ findIndex (/= 0) col) m)
   | x == 1 = let M (r:rs) = foldr (\k -> addRow k 0 (negate $ entry (k,0) m)) m [1..h-1]
                  M ts = forward (M rs)
              in M (r:ts)
   | otherwise = forward (scaleRow 0 (1/x) m)
 where
   (h, w) = dimensions m
   x      = entry (0,0) m
   col    = column 0 m

backward :: Fractional a => Matrix a -> Matrix a
backward m = foldr f m [1..h-1]
 where
   (h, _) = dimensions m
   f i    = let g j = case findIndex (/=0) (row i m) of
                         Just k  -> addRow j i (negate (entry (j, k) m))
                         Nothing -> id
            in flip (foldr g) [0..i-1]

rank :: Fractional a => Matrix a -> Int
rank = length . filter (isJust . pivot) . rows . reduce

nullity :: Fractional a => Matrix a -> Int
nullity m = snd (dimensions m) - rank m 
 
inverse :: Fractional a => Matrix a -> Maybe (Matrix a)
inverse m
   | h /= w     = Nothing
   | rank m < w = Nothing
   | otherwise  = Just $ M $ map (drop h) $ rows $ reduce $ M $ zipWith (++) (rows m) $ rows $ identity h
 where 
   (h, w) = dimensions m

invertible :: Fractional a => Matrix a -> Bool
invertible = isJust . inverse

eqMatrix :: Fractional a => Matrix a -> Matrix a -> Bool
eqMatrix m1 m2 = reduce m1 == reduce m2

-- test = rank $ makeMatrix $ [[0 :: Rational ,1,1,1], [1,2,3,2], [3,1,1,3]]

-- t = inverse $ M [[1,0],[0,3]]

-------------------------------------------------------

transpose :: Matrix a -> Matrix a
transpose (M rs) = M (L.transpose rs)

-------------------------------------------------------

isSquare :: Matrix a -> Bool
isSquare m = i==j
 where (i, j) = dimensions m

identityMatrix :: Num a => Int -> Matrix a
identityMatrix n = M $ map (\y -> map (\x -> if x==y then 1 else 0) list) list 
 where list = [0..n-1]

-------------------------------------------------------
-- Elementary row operations (preserve matrix equivalence)

checkRow :: Int -> Matrix a -> Bool
checkRow i m = i >= 0 && i < fst (dimensions m)

switchRows :: Int -> Int -> Matrix a -> Matrix a
switchRows i j m@(M rs)
   | i == j = m
   | i >  j = switchRows j i m
   | checkRow i m && checkRow j m = 
        let (before, r1:rest)  = splitAt i       rs
            (middle, r2:after) = splitAt (j-i-1) rest
        in M $ before ++ [r2] ++ middle ++ [r1] ++ after
   | otherwise = 
        error "switchRows: invalid rows"

scaleRow :: Num a => Int -> a -> Matrix a -> Matrix a
scaleRow i a m@(M rs)
   | checkRow i m = 
        let f y = if y==i then map (*a) else id
        in M $ zipWith f [0..] rs
   | otherwise = 
        error "scaleRow: invalid row"

addRow :: Num a => Int -> Int -> a -> Matrix a -> Matrix a
addRow i j a m@(M rs) 
   | checkRow i m && checkRow j m = 
        let rj  = map (*a) (row j m)
            f y = if y==i then zipWith (+) rj else id
        in M $ zipWith f [0..] rs
   | otherwise = 
        error "addRow: invalid row"

-------------------------------------------------------

isLowerTriangular :: Num a => Matrix a -> Bool
isLowerTriangular = and . zipWith check [1..] . rows
 where check n = all (==0) . drop n

isUpperTriangular :: Num a => Matrix a -> Bool
isUpperTriangular = and . zipWith check [0..] . rows
 where check n = all (==0) . take n

inRowEchelonForm :: Num a => Matrix a -> Bool
inRowEchelonForm (M rs) =
   null (filter nonZero (dropWhile nonZero rs)) &&
   increasing (map (length . takeWhile (==0)) (filter nonZero rs))
 where
   increasing (x:ys@(y:_)) = x < y && increasing ys
   increasing _ = True

nonZero :: Num a => [a] -> Bool
nonZero = any (/=0)

-- or row canonical form
inRowReducedEchelonForm :: Num a => Matrix a -> Bool
inRowReducedEchelonForm m@(M rs) =
   inRowEchelonForm m && 
   all (==1) (mapMaybe pivot rs) &&
   all (isPivotColumn . flip column m . length . takeWhile (==0)) (filter nonZero rs)

pivot :: Num a => Row a -> Maybe a
pivot r = case dropWhile (==0) r of
             hd:_ -> Just hd
             _    -> Nothing

isPivotColumn :: Num a => Column a -> Bool
isPivotColumn c = 
   case filter (/=0) c of
      [1] -> True
      _   -> False