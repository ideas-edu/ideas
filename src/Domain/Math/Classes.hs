module Domain.Math.Classes where

import Common.Context
import Control.Monad
import Data.List
import qualified Data.IntMap as IM

class Symbolic a where
   variable :: String -> a
   symbol   :: String -> a
   function :: String -> [a] -> a
   -- default definition
   symbol s = function s []
   
class MetaVar a where
   metaVar   :: Int -> a
   isMetaVar :: a -> Maybe Int
   
class NF a where
   normalize :: a -> a

class Constructor a where
   constructor :: a -> String

class ShallowEq a where 
   shallowEq :: a -> a -> Bool

--------

type Subst a = IM.IntMap a

(|->) :: (MetaVar a, Uniplate a) => Subst a -> a -> a
s |-> e = 
   case isMetaVar e of
      Just i  -> maybe e id $ IM.lookup i s
      Nothing -> let (cs, f) = uniplate e
                 in f (map (s |->) cs)
 
freeVars :: (MetaVar a, Uniplate a) => a -> [Int]
freeVars e = nub [ i | a <- universe e, Just i <- [isMetaVar a] ]

nextVar :: (Uniplate a, MetaVar a) => a -> Int
nextVar a = 1 + maximum (-1 : freeVars a)

renumberVars :: (Uniplate a, MetaVar a) => a -> a
renumberVars a = s |-> a
 where s = IM.fromList $ zip (freeVars a) (map metaVar [0..])
 
class (Constructor a, ShallowEq a, Uniplate a) => UniplateConstr a
   
unify :: (MonadPlus m, MetaVar a, UniplateConstr a) => a -> a -> m (Subst a)
unify x y = 
   case (isMetaVar x, isMetaVar y) of
      (Just i, Just j) | i==j -> return IM.empty
      (Just i, _) | i `notElem` freeVars y -> return $ IM.singleton i y
      (_, Just j) | j `notElem` freeVars x -> return $ IM.singleton j x
      _ | shallowEq x y ->
             unifyList (children x) (children y)
        | otherwise ->
             fail "unify"

unifyList :: (MonadPlus m, MetaVar a, UniplateConstr a) => [a] -> [a] -> m (Subst a)
unifyList []     []     = return IM.empty
unifyList (x:xs) (y:ys) = do
   s1 <- unify x y
   let f = map (s1 |->)
   s2 <- unifyList (f xs) (f ys)
   return (s1 `IM.union` s2)
unifyList _ _ = fail "unify"
