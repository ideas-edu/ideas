{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Ideas.Common.Traversal.Utils
   ( -- * Update type class
     Update(..), current, change, replace, changeM, changeG
     -- * Focus type class
   , Focus(..), liftFocus, unliftFocus
     -- * Wrapper type class
   , Wrapper(..), liftWrapper, unliftWrapper, mapWrapper
     -- * Mirror
   , Mirror, makeMirror
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

changeM :: Update f => (a -> Maybe a) -> f a -> Maybe (f a)
changeM = changeG

changeG :: (Update f, Monad g) => (a -> g a) -> f a -> g (f a)
changeG f a = (`replace` a) <$> f (current a)

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
liftFocus f = fmap focus . f . unfocus

unliftFocus :: Focus a => (a -> Maybe a) -> Unfocus a -> Maybe (Unfocus a)
unliftFocus f = fmap unfocus . f . focus

---------------------------------------------------------------
-- Wrapper type class

class Wrapper f where
   wrap   :: a -> f a
   unwrap :: f a -> a

liftWrapper :: (Monad m, Wrapper f) => (a -> m a) -> f a -> m (f a)
liftWrapper f = fmap wrap . f . unwrap

unliftWrapper :: (Monad m, Wrapper f) => (f a -> m (f a)) -> a -> m a
unliftWrapper f = fmap unwrap . f . wrap

mapWrapper :: Wrapper f => (a -> a) -> f a -> f a
mapWrapper f = wrap . f . unwrap

---------------------------------------------------------------
-- Mirror

newtype Mirror a = Mirror { fromMirror :: a }
   deriving (Show, Eq)

instance Wrapper Mirror where
   wrap   = Mirror
   unwrap = fromMirror

makeMirror :: a -> Mirror a
makeMirror = wrap

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