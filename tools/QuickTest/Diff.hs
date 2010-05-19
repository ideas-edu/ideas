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
-- A very simple implementation of line-based difference
--
-----------------------------------------------------------------------------
module Diff (diff) where

import Data.List (isInfixOf)

diff :: (String, String, [String]) -> String -> String -> String
diff (title1, title2, without) xs ys = 
   unlines $ concatMap present $ diffList (==) (f xs) (f ys)
 where
   f = filter p . number . lines
   p (_, s) = all (not . (`isInfixOf` s)) without

   present = either (block title1 ">  ") (block title2 "<  ")
   block title pre xs = (title ++ line xs) : map ((pre++) . snd) xs
   line xs = let i  = fst (head xs)
                 n  = length xs - 1
                 to = if n==0 then "" else "-" ++ show (i+n)
             in " (line " ++ show i ++ to ++"):"
   
diffList :: (a -> b -> Bool) -> [a] -> [b] -> [Either [a] [b]]
diffList eq = rec
 where
   rec lista@(a:as) listb@(b:bs)
      | a `eq` b  = rec as bs
      | otherwise =
           make Left a1 ++ make Right b1 ++ rec a2 b2
    where
      ps  = pairs (number lista) (number listb)
      fps = filter (\((_, a), (_, b)) -> a `eq` b) ps
      (da, db) = case fps of
                    [] -> (length lista, length listb)
                    ((i, _), (j, _)):_ -> (i-1, j-1)
      (a1, a2) = splitAt da lista
      (b1, b2) = splitAt db listb
      
   rec as bs = make Left as ++ make Right bs
    
   make f xs = [ f xs | not (null xs) ]

pairs :: [a] -> [b] -> [(a, b)]
pairs as = concat . foldr combine []
 where
   combine b bs = [ (a, b) | a <- as ] +++ ([] : bs)

   (x:xs) +++ (y:ys) = (x:y) : (xs +++ ys)
   []     +++ ys     = ys
   xs     +++ []     = map return xs
   
number :: [a] -> [(Int, a)]
number = zip [1..]