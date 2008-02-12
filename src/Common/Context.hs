-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Common.Context where

import Common.Utils
import Control.Monad
import Data.Dynamic
import qualified Data.Map as M

data Context a = Context
   { environment :: M.Map String Dynamic
   , location    :: [Int]
   , fromContext :: a
   }
 deriving Show

inContext :: a -> Context a
inContext = Context M.empty []

instance Eq a => Eq (Context a) where
   x == y = fromContext x == fromContext y 

instance Ord a => Ord (Context a) where
   x `compare` y = fromContext x `compare` fromContext y

instance Functor Context where
   fmap f c = c { fromContext = f (fromContext c) }

-- a variable has a name (for showing) and a default value (for initializing)
data Var a = String := a

get :: Typeable a => Var a -> Context b -> a
get (s := a) = maybe a (flip fromDyn a) . M.lookup s . environment

set :: Typeable a => Var a -> a -> Context b -> Context b
set (s := _) a mic = mic {environment = M.insert s (toDyn a) (environment mic)}

change :: Typeable a => Var a -> (a -> a) -> Context b -> Context b
change v f c = set v (f (get v c)) c

---------------------------------------------------------
-- Uniplate class for generic traversals

class Uniplate a where
   uniplate :: a -> ([a], [a] -> a)

noUniplate :: a -> ([a], [a] -> a)
noUniplate a = ([], const a)

children :: Uniplate a => a -> [a]
children = fst . uniplate

child :: Uniplate a => Int -> a -> Maybe a
child n = safeHead . drop n . children 
               
select :: Uniplate a => [Int] -> a -> Maybe a
select = flip $ foldM $ flip child

transform :: Uniplate a => Int -> (a -> a) -> a -> a
transform n f a = 
   let (as, build) = uniplate a 
       g i = if i==n then f else id
   in build (zipWith g [0..] as)

transformAt :: Uniplate a => [Int] -> (a -> a) -> a -> a
transformAt = flip (foldr transform)