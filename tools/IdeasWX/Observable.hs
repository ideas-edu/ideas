{-# OPTIONS -XMultiParamTypeClasses 
            -XFunctionalDependencies
            -XFlexibleInstances #-}
-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Observable 
   ( Observable(..) -- exports the type class and its member functions
   , changeValue, addObserver_, notifyObservers
   , Control -- exports the type Control, but not its constructor/selector functions
   , createControl
   ) where

import Data.IORef

-- A type class for observable objects
-- For an example, see the "instance Observable (Control a) a"
class Observable obs val | obs -> val where
   getValue     :: obs -> IO val
   setValue     :: obs -> val -> IO ()
   getObservers :: obs -> IO [val -> IO ()]
   addObserver  :: obs -> (val -> IO ()) -> IO ()

changeValue :: Observable obs val => obs -> (val -> val) -> IO ()
changeValue control f = 
   do a <- getValue control
      setValue control (f a)

addObserver_  :: Observable obs val => obs -> IO () -> IO ()
addObserver_ obs = 
   addObserver obs . const

notifyObservers :: Observable obs val => obs -> IO ()
notifyObservers obs = 
   do val     <- getValue obs
      obsList <- getObservers obs
      mapM_ ($ val) obsList

-- A "control" contains:
-- 1) a pointer to the current value
-- 2) a pointer to a list of "notify" functions (of the observers)
data Control a = C 
   { model     :: IORef a
   , observers :: IORef [a -> IO ()]
   }

-- The "smart" constructor function of a control.
-- Notice that the actual constructor "C" is not visible outside this module
createControl :: a -> IO (Control a)
createControl a =
   do m   <- newIORef a  
      obs <- newIORef []
      return (C m obs) 

instance Observable (Control a) a where 

   getValue = readIORef . model
   
   setValue ctrl a = 
      do writeIORef (model ctrl) a
         notifyObservers ctrl
   
   getObservers = readIORef . observers
   
   addObserver ctrl callback = 
      modifyIORef (observers ctrl) (callback:)