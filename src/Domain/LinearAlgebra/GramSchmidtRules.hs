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
import Common.Navigator hiding (current)
import Common.Utils
import Domain.LinearAlgebra.Vector
import Control.Monad
import Data.List

varI, varJ :: Var Int
varI = newVar "considered" 0
varJ = newVar "j" 0

rulesGramSchmidt :: Floating a => [Rule (Context (VectorSpace a))]
rulesGramSchmidt = [ruleNormalize, ruleOrthogonal, ruleNext]

-- Make the current vector of length 1
-- (only applicable if this is not already the case)
ruleNormalize :: Floating a => Rule (Context (VectorSpace a))
ruleNormalize = makeSimpleRule "Turn into unit Vector" $ withCM $ \vs -> do
   v <- current vs
   guard (norm v `notElem` [0, 1])
   setCurrent (toUnit v) vs

-- Make the current vector orthogonal with some other vector
-- that has already been considered
ruleOrthogonal :: Floating a => Rule (Context (VectorSpace a))
ruleOrthogonal = makeRule "Make orthogonal" $ supplyLabeled2 descr args transOrthogonal
 where
   descr = ("vector 1", "vector 2")
   args  = evalCM $ \_ -> do
              i <- liftM pred (readVar varI)
              j <- liftM pred (readVar varJ)
              guard (i>j)
              return (j, i)

-- Variable "j" is for administrating which vectors are already orthogonal 
ruleNextOrthogonal :: Rule (Context (VectorSpace a))
ruleNextOrthogonal = minorRule $ makeSimpleRule "Orthogonal to next" $ withCM $ \vs -> do
   i <- readVar varI
   j <- liftM succ (readVar varJ)
   guard (j < i)
   writeVar varJ j
   return vs

-- Consider the next vector 
-- This rule should fail if there are no vectors left
ruleNext :: Rule (Context (VectorSpace a))
ruleNext = minorRule $ makeSimpleRule "Consider next vector" $ withCM $ \vs -> do
   i <- readVar varI
   guard (i < length (vectors vs))
   writeVar varI (i+1)
   writeVar varJ 0
   return vs

current :: VectorSpace a -> ContextMonad (Vector a)
current vs = do
   i <- readVar varI
   case drop (i-1) (vectors vs) of
      v:_ -> return v
      _   -> mzero

setCurrent :: Vector a -> VectorSpace a -> ContextMonad (VectorSpace a)
setCurrent v vs = do
   i <- readVar varI 
   case splitAt (i-1) (vectors vs) of
      (xs, _:ys) -> return $ makeVectorSpace (xs ++ v:ys)
      _          -> mzero

-- Two indices, change the second vector and make it orthogonal
-- to the first
transOrthogonal :: Floating a => Int -> Int -> Transformation (Context (VectorSpace a))
transOrthogonal i j = contextTrans $ \xs ->
   do guard (i /= j && i >=0 && j >= 0)
      u <- safeHead $ drop i (vectors xs)
      guard (isUnit u)
      case splitAt j (vectors xs) of
         (begin, v:end) -> Just $ makeVectorSpace $ begin ++ makeOrthogonal u v:end
         _ -> Nothing 

-- Find proper abstraction, and move this function to transformation module
contextTrans :: (a -> Maybe a) -> Transformation (Context a)
contextTrans f = makeTrans $ \c -> do
   a   <- fromContext c
   new <- f a
   return (replace new c)