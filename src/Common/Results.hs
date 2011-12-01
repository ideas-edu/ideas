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
   ( ApplyResults(..), Results
   , resultsEnvironment, setEnvironment, addEnvironment, runResults
   , localBinding, toResults, fromResults
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

newtype Results a = R (StateT Environment [] a)
   deriving (Functor, Monad, MonadPlus)

resultsEnvironment :: Results Environment
resultsEnvironment = R get

changeEnvironment :: (Environment -> Environment) -> Results ()
changeEnvironment = R . modify

addEnvironment :: Environment -> Results ()
addEnvironment = changeEnvironment . mappend

setEnvironment :: Environment -> Results ()
setEnvironment = changeEnvironment . const

runResults :: Environment -> Results a -> [(a, Environment)]
runResults env (R m) = runStateT m env

localBinding :: Typeable a => Binding a -> Results ()
localBinding = changeEnvironment . insertBinding

toResults :: Foldable f => f a -> Results a
toResults = msum . map return . toList

fromResults :: Results a -> [a]
fromResults = map fst . runResults mempty
