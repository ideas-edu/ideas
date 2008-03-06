module Domain.LinearAlgebra.GramSchmidtRules where

import Common.Context
import Common.Transformation
import Common.Utils
import Domain.LinearAlgebra.Vector
import Control.Monad
import Data.List

varI :: Var Int
varI = "considered" := 0

rulesGramSchmidt :: Floating a => [Rule (Context [Vector a])]
rulesGramSchmidt = [ruleNormalize, ruleOrthogonal, ruleNext]

-- Make the current vector of length 1
-- (only applicable if this is not already the case)
ruleNormalize :: Floating a => Rule (Context [Vector a])
ruleNormalize = makeSimpleRule "Turn into unit Vector" $
   \c -> do v <- current c
            guard (norm v `notElem` [0, 1])
            if norm (toUnit v) /= 1 then error $ "NEE!" ++ show v ++ show (toUnit v) ++ show (norm v) else return ()
            setCurrent (toUnit v) c

-- Make the current vector orthogonal with some other vector
-- that has already been considered
ruleOrthogonal :: Num a => Rule (Context [Vector a])
ruleOrthogonal = makeRule "Make orthogonal" $ supplyLabeled2 descr args transOrthogonal
 where
   descr  = ("vector 1", "vector 2")
   args c = do let xs = take (get varI c - 1) (fromContext c)
               v <- current c 
               i <- findIndex (not . orthogonal v) xs
               return ( i, get varI c - 1)

-- Consider the next vector 
-- This rule should fail if there are no vectors left
ruleNext :: Rule (Context [Vector a])
ruleNext = minorRule $ makeSimpleRule "Consider next vector" $
   \c -> do guard (get varI c < length (fromContext c))
            return $ change varI (+1) c 

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
transOrthogonal i j = contextTrans $ \xs ->
   do guard (i /= j)
      u <- safeHead $ drop i xs
      case splitAt j xs of
         (begin, v:end) -> if not (orthogonal u (makeOrthogonal u v)) then error "FOUTJE" else 
                           Just $ begin ++ makeOrthogonal u v:end
         _ -> Nothing 

-- Find proper abstraction, and move this function to transformation module
contextTrans :: (a -> Maybe a) -> Transformation (Context a)
contextTrans f = makeTrans $ \c -> do
   new <- f (fromContext c)
   return (fmap (const new) c)