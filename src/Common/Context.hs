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
import Common.Transformation -- get rid of this import
import Control.Monad
import Data.Dynamic
import qualified Data.Map as M

class Functor f => Context f where
   inContext   :: a -> f a
   fromContext :: f a -> a

eqContext :: (Context f, Eq a) => f a -> f a -> Bool
eqContext x y = fromContext x == fromContext y

compareContext :: (Context f, Ord a) => f a -> f a -> Ordering
compareContext x y = fromContext x `compare` fromContext y

---------------
-- Temp: clean up

data InContext a = InContext (M.Map String Dynamic) [Int] a
   deriving Show

instance Context InContext where
   inContext = InContext M.empty []
   fromContext (InContext _ _ a) = a

instance Eq a  => Eq  (InContext a) where (==)    = eqContext
instance Ord a => Ord (InContext a) where compare = compareContext

instance Functor InContext where
   fmap f (InContext env loc a) = InContext env loc (f a)

liftContextRule :: Uniplate a => Rule a -> Rule (InContext a)
liftContextRule = liftRule $ LiftPair getter setter
 where
   getter c   = select (location c) (fromContext c)
   setter p c = fmap (transformAt (location c) (const p)) c

location :: InContext a -> [Int]
location (InContext _ loc _) = loc

get :: Typeable a => Var a -> InContext b -> a
get (s := a) (InContext env loc _) = maybe a (flip fromDyn a) (M.lookup s env)

set :: Typeable a => Var a -> a -> InContext b -> InContext b
set (s := _) a (InContext env loc b) = InContext (M.insert s (toDyn a) env) loc b

change :: Typeable a => Var a -> (a -> a) -> InContext b -> InContext b
change v f c = set v (f (get v c)) c

---------------
{-
newtype And f g a = And (f (g a))
   deriving Show

instance (Context f, Context g) => Context (And f g) where
   inContext = And . inContext . inContext
   fromContext (And a) = fromContext (fromContext a)

instance (Ord a, Context f, Context g) => Eq  (And f g a) where (==)    = eqContext
instance (Ord a, Context f, Context g) => Ord (And f g a) where compare = compareContext

instance (Functor f, Functor g) => Functor (And f g) where
   fmap f (And a) = And (fmap (fmap f) a) 

-------------

data Env a = Env (M.Map String Dynamic) a
   deriving Show

instance Context Env where
   inContext = Env M.empty
   fromContext (Env _ a) = a

instance Eq a  => Eq  (Env a) where (==)    = eqContext
instance Ord a => Ord (Env a) where compare = compareContext

instance Functor Env where
   fmap f (Env env a) = Env env (f a)

liftEnv :: (a -> a) -> Env a -> Env a
liftEnv = fmap
-}
-- a variable has a name (for showing) and a default value (for initializing)
data Var a = String := a
{-
get :: Typeable a => Var a -> Env b -> a
get (s := a) (Env env _) = maybe a (flip fromDyn a) (M.lookup s env)

set :: Typeable a => Var a -> a -> Env b -> Env b
set (s := _) a (Env env b) = Env (M.insert s (toDyn a) env) b

change :: Typeable a => Var a -> (a -> a) -> Env b -> Env b
change v f c = set v (f (get v c)) c

---------------------------------------------------------
-- Uniplate class for generic traversals

data Loc a = Loc [Int] a
   deriving Show

instance Context Loc where
   inContext = Loc []
   fromContext (Loc _ a) = a

instance Eq a  => Eq  (Loc a) where (==)    = eqContext
instance Ord a => Ord (Loc a) where compare = compareContext

instance Functor Loc where
   fmap f (Loc loc a) = Loc loc (f a)

liftLoc :: Uniplate a => (a -> a) -> Loc a -> Loc a
liftLoc f (Loc loc a) = Loc loc (transformAt loc f a)
   
location :: Loc a -> [Int]
location (Loc loc _) = loc
-}
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