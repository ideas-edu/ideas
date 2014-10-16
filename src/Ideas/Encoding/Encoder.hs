{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
--  $Id: Evaluator.hs 6535 2014-05-14 11:05:06Z bastiaan $

module Ideas.Encoding.Encoder
   ( -- * Encoder datatype 
     Encoder
     -- * Constructors
   , makeEncoder, encoderFor, (//), encoders
     -- * Query the encoder's state
   , exerciseEncoder, withExercise, withOpenMath
     -- * Typed encoders
   , (<?>), encodeTyped
     -- * Running an encoder
   , runEncoder, runEncoderM, runEncoderOpenMath
     -- re-export
   , module Data.Monoid, module Control.Applicative
   , (C.>>>)
   ) where

import Control.Applicative hiding (Const)
import Control.Arrow
import qualified Control.Category as C
import Control.Monad
import Data.Monoid
import Ideas.Common.Library (Exercise)
import Ideas.Service.Types
import Ideas.Text.XML

-------------------------------------------------------------------
-- Data type and its internal state

newtype Encoder a t b = Enc { run :: forall m . Monad m => EncoderState a -> t -> m b }

data EncoderState a = MakeState
   { stateExercise :: Exercise a -- the current exercise
   , stateOpenMath :: Bool       -- use OpenMath encoding
   }

instance C.Category (Encoder a) where
   id    = arr id
   f . g = Enc $ \st -> run g st >=> run f st

instance Arrow (Encoder a) where
   arr f   = Enc $ const (return . f)
   first f = Enc $ \st (a, b) -> run f st a >>= \c -> return (c, b)

instance Functor (Encoder a t) where
   fmap = liftA

instance Applicative (Encoder a t) where
   pure    = arr . const
   f <*> g = Enc $ \st a -> liftM2 ($) (run f st a) (run g st a)

instance Monad (Encoder a t) where
   return  = pure
   fail s  = Enc $ \_ _ -> fail s
   m >>= f = Enc $ \st a -> run m st a >>= \b -> run (f b) st a

instance Monoid b => Monoid (Encoder a t b) where
   mempty  = pure mempty
   mappend = liftA2 (<>)

instance BuildXML b => BuildXML (Encoder a t b) where
   n .=. s   = pure (n .=. s)
   unescaped = pure . unescaped
   builder   = pure . builder
   tag       = liftA . tag

-------------------------------------------------------------------
-- Constructors

makeEncoder :: (t -> b) -> Encoder a t b
makeEncoder = arr

encoderFor :: (t -> Encoder a t b) -> Encoder a t b
encoderFor f = Enc $ \st a -> run (f a) st a

infixl 8 //

(//) :: Encoder a t1 b -> t1 -> Encoder a t2 b
f // a = arr (const a) >>> f

encoders :: [Encoder a t b] -> Encoder a t [b]
encoders = sequence

-------------------------------------------------------------------
-- Query the encoder's state

exerciseEncoder :: (Exercise a -> t -> b) -> Encoder a t b
exerciseEncoder f = withExercise $ \ex -> makeEncoder (f ex)

withExercise :: (Exercise a -> Encoder a t b) -> Encoder a t b
withExercise f = Enc $ \st -> run (f (stateExercise st)) st

withOpenMath :: (Bool -> Encoder a t b) -> Encoder a t b
withOpenMath f = Enc $ \st -> run (f (stateOpenMath st)) st

-------------------------------------------------------------------
-- Typed encoders

infixr 5 <?>

(<?>) :: Typed a1 t => Encoder a t b -> Encoder a (TypedValue (Type a1)) b
                                     -> Encoder a (TypedValue (Type a1)) b
f <?> g = Enc $ \st tv@(val ::: tp) -> 
   case equal tp typed of
      Nothing -> run g st tv
      Just to -> run f st (to val)

encodeTyped :: Typed a t => Encoder st t b -> Encoder st (TypedValue (Type a)) b
encodeTyped f = f <?> fail "Types do not match"

-------------------------------------------------------------------
-- Running an encoder
   
-- | Run an encoder
runEncoder :: Encoder a t b -> Exercise a -> t -> b
runEncoder enc ex = either error id . runEncoderWith False enc ex

-- | Run an encoder
runEncoderM :: Monad m => Encoder a t b -> Exercise a -> t -> m b
runEncoderM = runEncoderWith False

-- | Run an encoder in OpenMath mode
runEncoderOpenMath :: Monad m => Encoder a t b -> Exercise a -> t -> m b
runEncoderOpenMath = runEncoderWith True

-- helper
runEncoderWith :: Monad m => Bool -> Encoder a t b -> Exercise a -> t -> m b
runEncoderWith b f ex = run f (MakeState ex b)