{-# LANGUAGE TypeFamilies #-}
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
module Common.Traversal.Utils
   ( -- * Update type class
     Update(..), current, change, replace
     -- * Focus type class
   , Focus(..), liftFocus, unliftFocus
     -- * Wrapper type class
   , Wrapper(..), liftWrapper, unliftWrapper, mapWrapper
     -- * Utility functions
   , (>|<), safe, fixp, fixpl, mplus, (>=>)
   ) where

import Control.Monad
import Data.Maybe

---------------------------------------------------------------
-- Update type class

class Update f where
   update  :: f a -> (a, a -> f a)

current :: Update f => f a -> a
current  = fst . update 

change  :: Update f => (a -> a) -> f a -> f a
change f = (\(x, g) -> g (f x)) . update

replace :: Update f => a -> f a -> f a
replace  = change . const

---------------------------------------------------------------
-- Focus type class

class Focus a where
   type Unfocus a
   focus   :: Unfocus a -> a
   focusM  :: Unfocus a -> Maybe a
   unfocus :: a -> Unfocus a
   -- default definitions
   focus  = fromMaybe (error "no focus") . focusM
   focusM = Just . focus

liftFocus :: Focus a => (Unfocus a -> Maybe (Unfocus a)) -> a -> Maybe a
liftFocus f = liftM focus . f . unfocus

unliftFocus :: Focus a => (a -> Maybe a) -> Unfocus a -> Maybe (Unfocus a)
unliftFocus f = liftM unfocus . f . focus

---------------------------------------------------------------
-- Wrapper type class

class Wrapper f where
   wrap   :: a -> f a
   unwrap :: f a -> a

liftWrapper :: Wrapper f => (a -> Maybe a) -> f a -> Maybe (f a)
liftWrapper f = fmap wrap . f . unwrap

unliftWrapper :: Wrapper f => (f a -> Maybe (f a)) -> a -> Maybe a
unliftWrapper f = fmap unwrap . f . wrap

mapWrapper :: Wrapper f => (a -> a) -> f a -> f a
mapWrapper f = wrap . f . unwrap

---------------------------------------------------------------
-- Utility functions

infixr 0 >|<

(>|<) :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
(f >|< g) a = f a `mplus` g a

safe :: (a -> Maybe a) -> a -> a 
safe f a = fromMaybe a (f a)

fixp :: (a -> Maybe a) -> a -> a
fixp f = last . fixpl f

fixpl :: (a -> Maybe a) -> a -> [a]
fixpl f a = a : maybe [] (fixpl f) (f a)