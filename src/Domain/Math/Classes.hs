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

class (Constructor a, ShallowEq a, Uniplate a) => UniplateConstr a

------

freeVars :: (MetaVar a, Uniplate a) => a -> [Int]
freeVars e = nub [ i | a <- universe e, Just i <- [isMetaVar a] ]

nextVar :: (Uniplate a, MetaVar a) => a -> Int
nextVar a = 1 + maximum (-1 : freeVars a)