module Domain.Math.Classes where

import Common.Uniplate
import Data.List

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

class (Constructor a, ShallowEq a, Uniplate a) => UniplateConstr a

------

freeVars :: (MetaVar a, Uniplate a) => a -> [Int]
freeVars a = freeVarsList [a]

freeVarsList :: (MetaVar a, Uniplate a) => [a] -> [Int] -- generalize to crush
freeVarsList xs = nub [ i | x <- xs, a <- universe x, Just i <- [isMetaVar a] ]

nextVar :: (Uniplate a, MetaVar a) => a -> Int
nextVar a = nextVarList [a]

nextVarList :: (Uniplate a, MetaVar a) => [a] -> Int
nextVarList xs = 
   case concatMap freeVars xs of
      [] -> 0
      is -> 1 + maximum is