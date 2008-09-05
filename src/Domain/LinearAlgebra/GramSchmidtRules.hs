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
module Domain.LinearAlgebra.GramSchmidtRules where

import Common.Context
import Common.Transformation
import Common.Utils
import Domain.LinearAlgebra.Vector
import Control.Monad
import Data.List

varI, varJ :: Var Int
varI = "considered" := 0
varJ = "j"          := 0

rulesGramSchmidt :: Floating a => [Rule (Context [Vector a])]
rulesGramSchmidt = [ruleNormalize, ruleOrthogonal, ruleNext]

-- Make the current vector of length 1
-- (only applicable if this is not already the case)
ruleNormalize :: Floating a => Rule (Context [Vector a])
ruleNormalize = makeSimpleRule "Turn into unit Vector" $
   \c -> do v <- current c
            guard (norm v `notElem` [0, 1])
            setCurrent (toUnit v) c

-- Make the current vector orthogonal with some other vector
-- that has already been considered
ruleOrthogonal :: Num a => Rule (Context [Vector a])
ruleOrthogonal = makeRule "Make orthogonal" $ supplyLabeled2 descr args transOrthogonal
 where
   descr  = ("vector 1", "vector 2")
   args c = do let i = get varI c-1
                   j = get varJ c
               guard (i>j)
               return (j, i)
   {-
   args c = do let xs = take (get varI c - 1) (fromContext c)
               v <- current c 
               i <- findIndex (not . orthogonal v) xs
               return ( i, get varI c - 1) -}

-- Variable "j" is for administrating which vectors are already orthogonal 
ruleNextOrthogonal :: Rule (Context [Vector a])
ruleNextOrthogonal = minorRule $ makeSimpleRule "Orthogonal to next" $
   return . change varJ (+1)

-- Consider the next vector 
-- This rule should fail if there are no vectors left
ruleNext :: Rule (Context [Vector a])
ruleNext = minorRule $ makeSimpleRule "Consider next vector" $
   \c -> do guard (get varI c < length (fromContext c))
            return $ change varI (+1) $ set varJ 0 c 

current :: Context [Vector a] -> Maybe (Vector a)
current c = 
   case drop (get varI c - 1) (fromContext c) of
      v:_ -> Just v
      _   -> Nothing

setCurrent :: Vector a -> Context [Vector a] -> Maybe (Context [Vector a])
setCurrent v c = 
   case splitAt (get varI c - 1) (fromContext c) of
      (xs, _:ys) -> Just $ fmap (const (xs ++ v:ys)) c 
      _          -> Nothing

-- Two indices, change the second vector and make it orthogonal
-- to the first
transOrthogonal :: Num a => Int -> Int -> Transformation (Context [Vector a])
transOrthogonal i j = contextTrans "transOrthogonal" $ \xs ->
   do guard (i /= j)
      u <- safeHead $ drop i xs
      case splitAt j xs of
         (begin, v:end) -> Just $ begin ++ makeOrthogonal u v:end
         _ -> Nothing 

-- Find proper abstraction, and move this function to transformation module
contextTrans :: String -> (a -> Maybe a) -> Transformation (Context a)
contextTrans s f = makeTrans s $ \c -> do
   new <- f (fromContext c)
   return (fmap (const new) c)