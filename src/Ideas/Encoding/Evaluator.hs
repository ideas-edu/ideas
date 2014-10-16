{-# LANGUAGE GADTs, RankNTypes #-}
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
--  $Id$

module Ideas.Encoding.Evaluator
   ( DecoderState, simpleDecoder, decoderFor, withStateD, (///)
   , runDecoderStateM, setInput
     -- re-export
   , module Control.Applicative, sequenceA
   , module Data.Monoid
     -- old
   , Evaluator(..), evalService
   ) where

import Control.Applicative hiding (Const)
import Control.Monad
import Data.Monoid
import Data.Traversable
import Ideas.Common.Library (Exercise)
import Ideas.Service.Types
import Ideas.Text.XML

newtype DecoderState st inp a = 
   Dec { runDec :: st -> inp -> Either String (a, inp) }

state :: DecoderState st inp st
state = Dec (\st inp -> Right (st, inp))

input :: DecoderState st inp inp
input = Dec (\_ inp -> Right (inp, inp))

setInput :: inp -> DecoderState st inp ()
setInput inp = Dec (\_ _ -> Right ((), inp))

{-
instance Functor (DecoderState st inp) where
   fmap = liftM

instance Applicative (DecoderState st inp) where
   pure  = return
   (<*>) = liftM2 ($)
   
instance Alternative (DecoderState st inp) where
   empty = fail "DecoderState: empty"
   (<|>) = mplus -}

instance Monad (DecoderState st inp) where
   return a    = Dec $ \_ inp -> Right (a, inp)
   fail s      = Dec $ \_ _ -> Left s
   Dec f >>= g = Dec $ \st inp -> 
      case f st inp of
         Left err -> Left err
         Right (a, inp2) -> runDec (g a) st inp2

instance MonadPlus (DecoderState st inp) where
   mzero = fail "DecoderState: mzero"
   Dec f `mplus` Dec g = Dec $ \st inp -> 
      case (f st inp, g st inp) of
         (Right a, _)  -> Right a
         (_, Right a)  -> Right a
         (Left msg, _) -> Left msg

(///) :: DecoderState st a c -> a -> DecoderState st b c
Dec f /// a = Dec $ \st inp -> 
   case f st a of
      Right (c, _) -> Right (c, inp)
      Left msg     -> Left msg

runDecoderStateM :: Monad m => DecoderState st a b -> st -> a -> m b
runDecoderStateM f st = either fail (return . fst) . runDec f st

-- derived
simpleDecoder :: (a -> b) -> DecoderState st a b
simpleDecoder f = liftM f input

decoderFor :: (a -> DecoderState st a b) -> DecoderState st a b
decoderFor f = input >>= f

withStateD :: (st -> b) -> DecoderState st a b
withStateD f = liftM f state

---

type Encoder a t b = EncoderX (EncoderState a b) t b

newtype EncoderX st t b = Enc { runEncoderState :: st -> t -> b }

runEncoder :: Encoder a t b -> Exercise a -> (a -> b) -> t -> b
runEncoder = runEncoderWith False

runEncoderOM :: Encoder a t b -> Exercise a -> (a -> b) -> t -> b
runEncoderOM = runEncoderWith True

runEncoderWith :: Bool -> Encoder a t b -> Exercise a -> (a -> b) -> t -> b
runEncoderWith b enc ex f = runEncoderState enc (EncoderState ex b f)

data EncoderState a b = EncoderState
   { getExercise :: Exercise a
   , isOpenMath  :: Bool
   , encodeTerm  :: a -> b
   }

instance Functor (EncoderX st a) where
   fmap = liftA

instance Applicative (EncoderX st a) where
   pure a = Enc $ \_ _ -> a
   Enc f <*> Enc g = Enc $ \st a -> (f st a) (g st a)

instance Monoid b => Monoid (EncoderX st a b) where
   mempty  = pure mempty
   mappend = liftA2 (<>)

instance BuildXML b => BuildXML (EncoderX st a b) where
   n .=. s   = pure (n .=. s)
   unescaped = pure . unescaped
   builder   = pure . builder
   tag       = liftA . tag

withExercise :: (Exercise a -> Encoder a t b) -> Encoder a t b
withExercise f = encoderStateFor $ \st _ -> f (getExercise st)

withOpenMath :: (Bool -> Encoder a t b) -> Encoder a t b
withOpenMath f = encoderStateFor $ \st _ -> f (isOpenMath st)

withTermEncoder :: ((a -> b) -> Encoder a t b) -> Encoder a t b
withTermEncoder f = encoderStateFor $ \st _ -> f (encodeTerm st)

makeEncoder :: (a -> b) -> EncoderX st a b
makeEncoder = Enc . const

exerciseEncoder :: (Exercise st -> a -> b) -> Encoder st a b
exerciseEncoder f = withExercise $ \ex -> makeEncoder (f ex)

encoderFor :: (a -> EncoderX st a b) -> EncoderX st a b
encoderFor = encoderStateFor . const

encoderStateFor :: (st -> a -> EncoderX st a b) -> EncoderX st a b
encoderStateFor f = Enc $ \st a -> let Enc g = f st a in g st a

infixr 5 <?>

(<?>) :: Typed a t => EncoderX st t b -> EncoderX st (TypedValue (Type a)) b 
                                      -> EncoderX st (TypedValue (Type a)) b
Enc f <?> Enc g = Enc $ \st tv@(val ::: tp) -> 
   case equal tp typed of
      Nothing -> g st tv
      Just to -> f st (to val)

encodeTyped :: Typed a t => EncoderX st t b -> EncoderX st (TypedValue (Type a)) b
encodeTyped f = f <?> Enc (\_ _ -> error "Types do not match")

infixl 8 //

(//) :: EncoderX st a c -> a -> EncoderX st b c
Enc f // a = Enc $ \st _ -> f st a

-------------------------------------------------------------------

evalService :: Evaluator a b -> Service -> IO b
evalService f = eval f . serviceFunction

data Evaluator a b where
   Evaluator :: (TypedValue (Type a) -> IO b)  -- encoder
             -> (forall t . Type a t -> IO t)  -- decoder
             -> Evaluator a b

eval :: Evaluator a b -> TypedValue (Type a) -> IO b
eval f@(Evaluator enc dec) tv@(val ::: tp) =
   case tp of
      -- handle exceptions
      Const String :|: t ->
         either fail (\a -> eval f (a ::: t)) val
      -- uncurry function if possible
      t1 :-> t2 :-> t3 ->
         eval f (uncurry val ::: Pair t1 t2 :-> t3)
      t1 :-> t2 -> do
         a <- dec t1
         eval f (val a ::: t2)
      -- perform IO
      IO t -> do
         a <- val
         eval f (a ::: t)
      _ ->
         enc tv