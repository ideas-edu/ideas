module Service.EditDistance (editDistance, boundedEditDistance) where

import Control.Monad.ST (ST)
import Data.Array ((!))
import Data.Array.ST
import Data.List (tails)

-- See LevenshteinDistance algorithm from Wikipedia
boundedEditDistance :: Eq a => Int -> [a] -> [a] -> Int
boundedEditDistance bound xs ys = runSTArray m ! (lenXs, lenYs)
 where
   lenXs = length xs
   lenYs = length ys
 
   m :: ST s (STArray s (Int, Int) Int)
   m = do -- create array
          a <- newArray ((0, 0), (lenXs, lenYs)) bound
          
          -- initialize first column and row
          flip mapM_ [0 .. lenXs `min` (bound-1)] $ \i -> 
             writeArray a (i, 0) i
          flip mapM_ [1 .. lenYs `min` (bound-1)] $ \j -> 
             writeArray a (0, j) j
             
          -- write interesting fields
          let zs   = zip [1 ..] ys
              rows = zipWith (\n r -> take ((bound+n-1) `min` (bound*2-1)) r) [0..] $ replicate (bound-1) zs ++ tails zs
              list = [ (i, x, j, y) 
                     | (i, x, row) <- zip3 [1 ..] xs rows
                     , (j, y) <- row
                     -- , if j > i-bound then True else error $ show (i,j)
                     -- , if j < i+bound then True else error $ show (i,j)
                     ]
          flip mapM_ list $ \(i, x, j, y) -> do
             -- deletion
             v1 <- readArray a (i-1, j)
             -- insertion
             v2 <- readArray a (i, j-1)
             -- substitution
             v3 <- readArray a (i-1, j-1)
             let cost = if x==y then 0 else 1
                 best = (v1+1) `min` (v2+1) `min` (v3+cost) `min` bound
             writeArray a (i, j) best
          return a

editDistance :: Eq a => [a] -> [a] -> Int
editDistance = boundedEditDistance (maxBound `div` 2) {- divide by two to prevent overflow -}
                  
w = boundedEditDistance 10 [1,3,2,3,5] [1,3,3,2,5] 