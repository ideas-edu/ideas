module Common.Traversable 
   ( Once(..), Switch(..), OnceJoin(..), useOnceJoin
   ) where

import Control.Monad
import qualified Data.IntMap as IM
import qualified Data.Map as M

{- Examples:

once (^2) [1..3]
   ~>  [[1,2,3],[1,4,3],[1,2,9]]

onceM (\x -> [x+1, x^2]) [1..3]
   ~>  [[2,2,3],[1,2,3],[1,3,3],[1,4,3],[1,2,4],[1,2,9]]

onceJoin (\x -> [x+1, x^2]) [1..3]
   ~>  [[2,1,2,3],[1,3,4,3],[1,2,4,9]]

onceJoinM (\x -> [[x+1], [x^2, x^3]]) [1..3]
   ~>  [[2,2,3],[1,1,2,3],[1,3,3],[1,4,8,3],[1,2,4],[1,2,9,27]]
-}

-----------------------------------------------------------
-- * Type class |Once|

class Functor f => Once f where
   -- | Apply a function once in a given structure
   once :: (a -> a) -> f a -> [f a]
   -- | Apply a monadic function once in a given structure
   onceM :: MonadPlus m => (a -> m a) -> f a -> m (f a)
   
   -- default definition
   once f = onceM (return . f)

instance Once [] where
   onceM = useOnceJoin
   
instance Once Maybe where
   onceM = useOnceJoin

instance Eq a => Once (M.Map a) where
   onceM f m = liftM M.fromAscList (onceM g (M.toList m))
    where g (a, b) = liftM (\c -> (a, c)) (f b)

instance Once IM.IntMap where
   onceM f m = liftM IM.fromAscList (onceM g (IM.toList m))
    where g (a, b) = liftM (\c -> (a, c)) (f b)

useOnceJoin :: (OnceJoin f, MonadPlus m) => (a -> m a) -> f a -> m (f a)
useOnceJoin f = onceJoinM (liftM return . f)

-----------------------------------------------------------
-- * Type class |Switch|

class Functor f => Switch f where
   switch :: Monad m => f (m a) -> m (f a)
         
instance Switch [] where
   switch = sequence

instance Switch Maybe where
   switch = maybe (return Nothing) (liftM Just)

instance Eq a => Switch (M.Map a) where
   switch m = do
      let (ns, ms) = unzip (M.toList m)
      as <- sequence ms 
      return $ M.fromAscList $ zip ns as

instance Switch IM.IntMap where
   switch m = do
      let (ns, ms) = unzip (IM.toList m)
      as <- sequence ms 
      return $ IM.fromAscList $ zip ns as

-----------------------------------------------------------
-- * Type class |OnceJoin|

class (Once f, Monad f) => OnceJoin f where
   -- | Apply a function once in a given structure, join the result afterwards
   onceJoin :: (a -> f a) -> f a -> [f a]
   -- | Apply a monadic function once in a given structure, join the result afterwards
   onceJoinM :: MonadPlus m => (a -> m (f a)) -> f a -> m (f a)

   -- default definition
   onceJoin f = onceJoinM (return . f)

instance OnceJoin [] where
   onceJoinM _ []     = mzero 
   onceJoinM f (x:xs) = liftM (++xs) (f x) `mplus` liftM (x:) (onceJoinM f xs)

instance OnceJoin Maybe where
   onceJoinM f = maybe mzero f