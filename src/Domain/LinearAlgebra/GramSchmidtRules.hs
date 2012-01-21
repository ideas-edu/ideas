-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.GramSchmidtRules 
   ( ruleNext, ruleNextOrthogonal
   , ruleOrthogonal, ruleNormalize
   , rulesGramSchmidt
   ) where

import Common.Library hiding (current)
import Control.Monad
import Domain.LinearAlgebra.Vector
import qualified Common.Navigator as N

varI, varJ :: Ref Int
varI = makeRef "considered"
varJ = makeRef "j"

getVarI, getVarJ :: EnvMonad Int
getVarI = varI :? 0
getVarJ = varJ :? 0

rulesGramSchmidt :: (Floating a, Reference a) => [Rule (Context (VectorSpace a))]
rulesGramSchmidt = [ruleNormalize, ruleOrthogonal, ruleNext]

-- Make the current vector of length 1
-- (only applicable if this is not already the case)
ruleNormalize :: Floating a => Rule (Context (VectorSpace a))
ruleNormalize = makeRule "Turn into unit Vector" $ makeTransEnv $ \vs -> do
   v  <- current vs
   guard (norm v `notElem` [0, 1])
   setCurrent (toUnit v) vs

-- Make the current vector orthogonal with some other vector
-- that has already been considered
ruleOrthogonal :: (Floating a, Reference a) => Rule (Context (VectorSpace a))
ruleOrthogonal = makeRule "Make orthogonal" $ 
   supplyParameters transOrthogonal $ useEnv $ \_ -> do
      i <- getVarI
      j <- getVarJ
      guard (i>j)
      return (pred j, pred i)

-- Variable "j" is for administrating which vectors are already orthogonal
ruleNextOrthogonal :: Rule (Context (VectorSpace a))
ruleNextOrthogonal = minorRule $ makeRule "Orthogonal to next" $ 
   makeTransEnv $ same $ \_ -> do
      i <- getVarI
      j <- liftM succ getVarJ
      guard (j < i)
      varJ := j

-- Consider the next vector
-- This rule should fail if there are no vectors left
ruleNext :: Rule (Context (VectorSpace a))
ruleNext = minorRule $ makeRule "Consider next vector" $ 
   makeTransEnv $ same $ \vs -> do
      i <- getVarI
      guard (i < length (vectors vs))
      varI := i+1
      varJ := 0

-- Two indices, change the second vector and make it orthogonal
-- to the first
transOrthogonal :: (Reference a, Floating a) => ParamTrans (Int, Int) (Context (VectorSpace a))
transOrthogonal = parameter2 "vector 1" "vector 2" $ \i j -> 
   liftToContext $ makeTrans $ \a -> do
      guard (i /= j && i >=0 && j >= 0)
      let vs = vectors a
      u <- elementAt i vs
      guard (isUnit u)
      liftM makeVectorSpace $ changeAt j (makeOrthogonal u) vs
   
current :: VectorSpace a -> EnvMonad (Vector a)
current vs = do
   i <- getVarI
   elementAt (i-1) (vectors vs)

setCurrent :: Vector a -> VectorSpace a -> EnvMonad (VectorSpace a)
setCurrent v vs = do
   i <- getVarI
   liftM makeVectorSpace $ replaceAt (i-1) v $ vectors vs
   
useEnv :: (a -> EnvMonad b) -> Context a -> Maybe b
useEnv f c = N.current c >>= \a -> evalEnvMonad (f a) (environment c)

same :: (a -> EnvMonad b) -> a -> EnvMonad a
same f a = f a >> return a

elementAt :: Monad m => Int -> [a] -> m a
elementAt i as =
   case drop i as of
      hd:_ -> return hd
      _    -> fail "elementAt"

changeAt :: Monad m => Int -> (a -> a) -> [a] -> m [a]
changeAt i f as =
   case splitAt i as of
      (xs, y:ys) -> return (xs ++ f y : ys)
      _          -> fail "changeAt"

replaceAt :: Monad m => Int -> a -> [a] -> m [a]
replaceAt i = changeAt i . const