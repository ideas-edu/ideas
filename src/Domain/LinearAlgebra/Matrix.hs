-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.Matrix 
   ( Matrix, Row, Column, isRectangular, makeMatrix, mapWithPos
   , rows, row, columns, column, dimensions, entry
   , switchRows, scaleRow, addRow
   , inRowEchelonForm, inRowReducedEchelonForm
   , nonZeroRow, pivot, isPivotColumn
   ) where

import Data.Maybe
import Data.List hiding (transpose)
import qualified Data.List as L
import qualified Data.Map as M

-- Invariant: a matrix is always rectangular
newtype Matrix a = M [[a]]
   deriving (Eq, Show)

type Row    a = [a]
type Column a = [a]

instance Functor Matrix where 
   fmap f (M rows) = M (map (map f) rows)

-- Check whether the table is rectangular
isRectangular :: [[a]] -> Bool
isRectangular xss =
   case map length xss of
      []   -> True
      n:ns -> all (==n) ns

-- Constructor function that checks whether the table is rectangular
makeMatrix :: [Row a] -> Matrix a
makeMatrix rows
   | null (concat rows) = M []
   | isRectangular rows = M rows
   | otherwise          = error "makeMatrix: not rectangular"

rows :: Matrix a -> [Row a]
rows (M rows) = rows

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
mapWithPos f (M rows) = M $ zipWith g [0..] rows
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

transpose :: Matrix a -> Matrix a
transpose (M rows) = M (L.transpose rows)

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
switchRows i j m@(M rows)
   | i == j = m
   | i >  j = switchRows j i m
   | checkRow i m && checkRow j m = 
        let (before, r1:rest)  = splitAt i       rows
            (middle, r2:after) = splitAt (j-i-1) rest
        in M $ before ++ [r2] ++ middle ++ [r1] ++ after
   | otherwise = 
        error "switchRows: invalid rows"

scaleRow :: Num a => Int -> a -> Matrix a -> Matrix a
scaleRow i a m@(M rows)
   | checkRow i m = 
        let f y = if y==i then map (*a) else id
        in M $ zipWith f [0..] rows
   | otherwise = 
        error "scaleRow: invalid row"

addRow :: Num a => Int -> Int -> a -> Matrix a -> Matrix a
addRow i j a m@(M rows) 
   | checkRow i m && checkRow j m = 
        let rj  = map (*a) (row j m)
            f y = if y==i then zipWith (+) rj else id
        in M $ zipWith f [0..] rows
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
inRowEchelonForm (M rows) =
   null (filter nonZeroRow (dropWhile nonZeroRow rows)) &&
   increasing (map (length . takeWhile (==0)) (filter nonZeroRow rows))
 where
   increasing (x:ys@(y:_)) = x < y && increasing ys
   increasing _ = True

nonZeroRow :: Num a => Row a -> Bool
nonZeroRow = any (/=0)

-- or row canonical form
inRowReducedEchelonForm :: Num a => Matrix a -> Bool
inRowReducedEchelonForm m@(M rows) =
   inRowEchelonForm m && 
   all (==1) (catMaybes $ map pivot rows) &&
   all (isPivotColumn . flip column m . length . takeWhile (==0)) (filter nonZeroRow rows)

pivot :: Num a => Row a -> Maybe a
pivot r = case dropWhile (==0) r of
             hd:_ -> Just hd
             _    -> Nothing

isPivotColumn :: Num a => Column a -> Bool
isPivotColumn c = 
   case filter (/=0) c of
      [1] -> True
      _   -> False

-------------------------------------------------------
-- m1 = makeMatrix [[0,1,4,0,3],[0,0,0,1,0],[0,0,0,0,1],[0,0,0,0,0]]