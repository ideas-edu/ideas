{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
module Common.Results 
   ( -- * Type and type class
     ApplyResults(..), ToResults(..), Results
     -- * Conversion
   , runResults, fromResults
     -- * Global environment
   , globalBinding, getGlobals, addGlobalEnvironment, setGlobals
     -- * Local environment
   , localBinding, getLocals, addLocalEnvironment
   ) where

import Common.Binding
import Common.Classes
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Typeable
import Data.Monoid

-----------------------------------------------------------
--- Transformations

class Apply f => ApplyResults f where
   applyResults :: f a -> a -> Results a

class ToResults f where
   toResults :: f a -> Results a 

instance ToResults Results where
   toResults = id

instance ToResults [] where
   toResults = msum . map return

instance ToResults Maybe where
   toResults = toResults . maybeToList

newtype Results a = R (StateT St [] a)
   deriving (Functor, Monad, MonadPlus)
   
data St = St {local :: Environment, global :: Environment}

getGlobals :: Results Environment
getGlobals = R $ gets global

changeGlobal :: (Environment -> Environment) -> Results ()
changeGlobal f = R $ modify $ \st -> st {global = f (global st)}

setGlobals :: Environment -> Results ()
setGlobals = changeGlobal . const

runResults :: Environment -> Results a -> [a]
runResults env (R m) = evalStateT m (St mempty env)

globalBinding :: Typeable a => Binding a -> Results ()
globalBinding = changeGlobal . insertBinding

addGlobalEnvironment :: Environment -> Results ()
addGlobalEnvironment = changeGlobal . mappend

fromResults :: Results a -> [a]
fromResults = runResults mempty

changeLocal :: (Environment -> Environment) -> Results ()
changeLocal f = R $ modify $ \st -> st {local = f (local st)}

localBinding :: Typeable a => Binding a -> Results ()
localBinding = changeLocal . insertBinding

addLocalEnvironment :: Environment -> Results ()
addLocalEnvironment = changeLocal . mappend

getLocals :: Results Environment
getLocals = R $ gets local

{-
class Monad m => X m where
   readBinding  :: Binding a -> m a
   writeBinding :: Binding a -> a -> m ()

instance X Results where
   readBinding = 

modifyBinding :: X m => Binding a -> (a -> a) -> m ()
modifyBinding a f = readBinding a >>= writeBinding a . f -}