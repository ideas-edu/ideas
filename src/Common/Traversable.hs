-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Common.Traversable 
   ( Switch(..), Crush(..)
   ) where

import Control.Monad.Identity
import qualified Data.IntMap as IM 
import qualified Data.Map as M

-----------------------------------------------------------
-- * Type class |Switch|

class Functor f => Switch f where
   switch :: Monad m => f (m a) -> m (f a)
         
instance Switch [] where
   switch = sequence

instance Switch Maybe where
   switch = maybe (return Nothing) (liftM Just)

instance Switch Identity where
   switch (Identity m) = liftM Identity m

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
-- * Type class |Crush|

class Functor f => Crush f where
   crush :: f a -> [a]

instance Crush [] where
   crush = id

instance Crush Maybe where
   crush = maybe [] return

instance Crush Identity where
   crush = return . runIdentity

instance Crush (M.Map a) where
   crush = M.elems

instance Crush IM.IntMap where
   crush = IM.elems