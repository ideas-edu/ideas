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
     ApplyResults(..), Results
     -- * Conversion
   , toResults, runResults, fromResults
     -- * Global environment
   , globalBinding, getGlobals, addGlobalEnvironment, setGlobals
     -- * Local environment
   , localBinding, getLocals, addLocalEnvironment
   ) where

import Common.Binding
import Common.Classes
import Control.Monad
import Control.Monad.State
import Data.Foldable (Foldable, toList)
import Data.Typeable
import Data.Monoid

-----------------------------------------------------------
--- Transformations

class Apply f => ApplyResults f where
   applyResults :: f a -> a -> Results a

newtype Results a = R (StateT St [] a)
   deriving (Functor, Monad, MonadPlus)
   
data St = St {local :: Environment, global :: Environment}

getGlobals :: Results Environment
getGlobals = R $ gets global

changeGlobal :: (Environment -> Environment) -> Results ()
changeGlobal f = R $ modify $ \st -> st {global = f (global st)}

setGlobals :: Environment -> Results ()
setGlobals = changeGlobal . const

runResults :: Environment -> Results a -> [(a, Environment)]
runResults env (R m) = map (mapSecond global) $ runStateT m (St mempty env)

globalBinding :: Typeable a => Binding a -> Results ()
globalBinding = changeGlobal . insertBinding

addGlobalEnvironment :: Environment -> Results ()
addGlobalEnvironment = changeGlobal . mappend

toResults :: Foldable f => f a -> Results a
toResults = msum . map return . toList

fromResults :: Results a -> [a]
fromResults = map fst . runResults mempty

changeLocal :: (Environment -> Environment) -> Results ()
changeLocal f = R $ modify $ \st -> st {local = f (local st)}

localBinding :: Typeable a => Binding a -> Results ()
localBinding = changeLocal . insertBinding

addLocalEnvironment :: Environment -> Results ()
addLocalEnvironment = changeLocal . mappend

getLocals :: Results Environment
getLocals = R $ gets local