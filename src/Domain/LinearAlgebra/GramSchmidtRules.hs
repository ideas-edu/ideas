-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
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

rulesGramSchmidt :: Floating a => [Rule (Context (VectorSpace a))]
rulesGramSchmidt = [ruleNormalize, ruleOrthogonal, ruleNext]

-- Make the current vector of length 1
-- (only applicable if this is not already the case)
ruleNormalize :: Floating a => Rule (Context (VectorSpace a))
ruleNormalize = makeSimpleRule "Turn into unit Vector" $
   \c -> do v <- current c
            guard (norm v `notElem` [0, 1])
            setCurrent (toUnit v) c

-- Make the current vector orthogonal with some other vector
-- that has already been considered
ruleOrthogonal :: Floating a => Rule (Context (VectorSpace a))
ruleOrthogonal = makeRule "Make orthogonal" $ supplyLabeled2 descr args transOrthogonal
 where
   descr  = ("vector 1", "vector 2")
   args c = do let i = get varI c-1
                   j = get varJ c-1
               guard (i>j)
               return (j, i)

-- Variable "j" is for administrating which vectors are already orthogonal 
ruleNextOrthogonal :: Rule (Context (VectorSpace a))
ruleNextOrthogonal = minorRule $ makeSimpleRule "Orthogonal to next" $
   \c -> do guard (get varJ c + 1 < get varI c)
            return (change varJ (+1) c)

-- Consider the next vector 
-- This rule should fail if there are no vectors left
ruleNext :: Rule (Context (VectorSpace a))
ruleNext = minorRule $ makeSimpleRule "Consider next vector" $
   \c -> do guard (get varI c < length (vectors (fromContext c)))
            return $ change varI (+1) $ set varJ 0 c 

current :: Context (VectorSpace a) -> Maybe (Vector a)
current c = 
   case drop (get varI c - 1) (vectors (fromContext c)) of
      v:_ -> Just v
      _   -> Nothing

setCurrent :: Vector a -> Context (VectorSpace a) -> Maybe (Context (VectorSpace a))
setCurrent v c = 
   case splitAt (get varI c - 1) (vectors (fromContext c)) of
      (xs, _:ys) -> Just $ fmap (makeVectorSpace . const (xs ++ v:ys)) c 
      _          -> Nothing

-- Two indices, change the second vector and make it orthogonal
-- to the first
transOrthogonal :: Floating a => Int -> Int -> Transformation (Context (VectorSpace a))
transOrthogonal i j = contextTrans "transOrthogonal" $ \xs ->
   do guard (i /= j && i >=0 && j >= 0)
      u <- safeHead $ drop i (vectors xs)
      guard (isUnit u)
      case splitAt j (vectors xs) of
         (begin, v:end) -> Just $ makeVectorSpace $ begin ++ makeOrthogonal u v:end
         _ -> Nothing 

-- Find proper abstraction, and move this function to transformation module
contextTrans :: String -> (a -> Maybe a) -> Transformation (Context a)
contextTrans s f = makeTrans s $ \c -> do
   new <- f (fromContext c)
   return (fmap (const new) c)