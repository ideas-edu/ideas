{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Ideas.Encoding.Encoder
   ( -- * Converter type class
     Converter(..)
   , getExercise, getStdGen, getScript, getRequest
   , withExercise, withOpenMath, (//)
     -- * Options
   , Options, simpleOptions, makeOptions
     -- * Encoder datatype
   , Encoder, TypedEncoder
   , makeEncoder, encoderFor, exerciseEncoder
   , (<?>), encodeTyped
     -- * Decoder datatype
   , Decoder, TypedDecoder
   , makeDecoder, decoderFor
   , split, symbol, setInput
     -- re-export
   , module Data.Monoid, module Control.Applicative
   , module Control.Arrow
   ) where

import Control.Applicative hiding (Const)
import Control.Arrow
import Control.Monad
import Data.Monoid
import Ideas.Common.Library hiding (exerciseId, symbol)
import Ideas.Common.Utils (Some(..))
import Ideas.Service.DomainReasoner
import Ideas.Service.FeedbackScript.Parser (parseScriptSafe, Script)
import Ideas.Service.Request
import Ideas.Service.Types
import Ideas.Text.XML
import System.Random (newStdGen, mkStdGen, StdGen)
import qualified Control.Category as C

-------------------------------------------------------------------
-- Converter type class

class Converter f where
   fromOptions :: (Options a -> t) -> f a s t
   run  :: Monad m => f a s t -> Options a -> s -> m t

getExercise :: Converter f => f a s (Exercise a)
getExercise = fromOptions exercise

getStdGen :: Converter f => f a s StdGen
getStdGen = fromOptions stdGen

getScript :: Converter f => f a s Script
getScript = fromOptions script

getRequest :: Converter f => f a s Request
getRequest = fromOptions request

withExercise :: (Converter f, Monad (f a s)) => (Exercise a -> f a s t) -> f a s t
withExercise = (getExercise >>=)

withOpenMath :: (Converter f, Monad (f a s)) => (Bool -> f a s t) -> f a s t
withOpenMath = (liftM useOpenMath getRequest >>=)

(//) :: (Converter f, Monad (f a s2)) => f a s t -> s -> f a s2 t
p // a = do
   xs <- fromOptions id
   run p xs a

-------------------------------------------------------------------
-- Options

data Options a = Options
   { exercise :: Exercise a -- the current exercise
   , request  :: Request    -- meta-information about the request
   , stdGen   :: StdGen     -- random number generator
   , script   :: Script     -- feedback script
   }

simpleOptions :: Exercise a -> Options a
simpleOptions ex =
   let req = emptyRequest {encoding = [EncHTML]}
   in Options ex req (mkStdGen 0) mempty

makeOptions :: DomainReasoner -> Request -> IO (Some Options)
makeOptions dr req = do
   Some ex  <-
      case exerciseId req of
         Just code -> findExercise dr code
         Nothing   -> return (Some emptyExercise)

   scr <- case feedbackScript req of
             Just s -> parseScriptSafe s
             Nothing
                | getId ex == mempty -> return mempty
                | otherwise          -> defaultScript dr (getId ex)
   stdgen <- newStdGen
   return $ Some Options
      { exercise = ex
      , request  = req
      , stdGen   = stdgen
      , script   = scr
      }

-------------------------------------------------------------------
-- Encoder datatype

newtype Encoder a s t = Enc { runEnc :: Options a -> s -> Error t }

type TypedEncoder a = Encoder a (TypedValue (Type a))

instance C.Category (Encoder a) where
   id    = arr id
   f . g = Enc $ \xs -> runEnc g xs >=> runEnc f xs

instance Arrow (Encoder a) where
   arr   f = Enc $ \_ -> return . f
   first f = Enc $ \xs (a, b) -> runEnc f xs a >>= \c -> return (c, b)

instance Alternative (Encoder a s) where
   empty = mzero
   (<|>) = mplus

instance Monad (Encoder a s) where
   return a = Enc $ \_ _ -> return a
   fail s   = Enc $ \_ _ -> fail s
   p >>= f  = Enc $ \xs s -> do
      (a) <- runEnc p xs s
      runEnc (f a) xs s

instance MonadPlus (Encoder a s) where
   mzero = fail "Decoder: mzero"
   mplus p q = Enc $ \xs s ->
      runEnc p xs s `mplus` runEnc q xs s

instance Functor (Encoder a s) where
   fmap = liftM

instance Applicative (Encoder a s) where
   pure  = return
   (<*>) = liftM2 ($)

instance Converter Encoder where
   fromOptions f = Enc $ \xs _ -> return (f xs)
   run f xs = runErrorM . runEnc f xs

instance Monoid t => Monoid (Encoder a s t) where
   mempty  = pure mempty
   mappend = liftA2 (<>)

instance BuildXML t => BuildXML (Encoder a s t) where
   n .=. s   = pure (n .=. s)
   unescaped = pure . unescaped
   builder   = pure . builder
   tag       = liftA . tag

makeEncoder :: (s -> t) -> Encoder a s t
makeEncoder = arr

encoderFor :: (s -> Encoder a s t) -> Encoder a s t
encoderFor f = C.id >>= f

exerciseEncoder :: (Exercise a -> s -> t) -> Encoder a s t
exerciseEncoder f = withExercise $ makeEncoder . f

infixr 5 <?>

(<?>) :: (Encoder a t b, Type a1 t) -> Encoder a (TypedValue (Type a1)) b
                                    -> Encoder a (TypedValue (Type a1)) b
(p, t) <?> q = do
   val ::: tp <- makeEncoder id
   case equal tp t of
      Just f -> p // f val
      Nothing -> q

encodeTyped :: Encoder st t b -> Type a t -> Encoder st (TypedValue (Type a)) b
encodeTyped p t = (p, t) <?> fail "Types do not match"

-------------------------------------------------------------------
-- Decoder datatype

newtype Decoder a s t = Dec { runDec :: Options a -> s -> Error (t, s) }

type TypedDecoder a s = forall t . Type a t -> Decoder a s t

instance Monad (Decoder a s) where
   return a = Dec $ \_ s -> return (a, s)
   fail s   = Dec $ \_ _ -> fail s
   p >>= f  = Dec $ \xs s1 -> do
      (a, s2) <- runDec p xs s1
      runDec (f a) xs s2

instance MonadPlus (Decoder a s) where
   mzero = fail "Decoder: mzero"
   mplus p q = Dec $ \xs s ->
      runDec p xs s `mplus` runDec q xs s

instance Functor (Decoder a s) where
   fmap = liftM

instance Applicative (Decoder a s) where
   pure  = return
   (<*>) = liftM2 ($)

instance Alternative (Decoder a s) where
   empty = fail "Decoder: empty"
   (<|>) = mplus

get :: Decoder a s s
get = Dec $ \_ s -> return (s, s)

put :: s -> Decoder a s ()
put s = Dec $ \_ _ -> return ((), s)

instance Converter Decoder where
   fromOptions f = Dec $ \xs s -> return (f xs, s)
   run f xs = liftM fst . runErrorM . runDec f xs

split :: (s -> Either String (t, s)) -> Decoder a s t
split f = get >>= either fail (\(a, s2) -> put s2 >> return a) . f

symbol :: Decoder a [s] s
symbol = split f
 where
   f []     = Left "Empty input"
   f (x:xs) = Right (x, xs)

setInput :: s -> Decoder a s ()
setInput inp = split (\_ -> Right ((), inp))

makeDecoder:: (s -> t) -> Decoder a s t
makeDecoder f = fmap f get

decoderFor :: (s -> Decoder a s t) -> Decoder a s t
decoderFor f = get >>= f

-------------------------------------------------------------------
-- Error monad (helper)

newtype Error a = Error { runError :: Either String a }

instance Functor Error where
   fmap = (<$>)

instance Applicative Error where
   pure  = return
   (<*>) = ap

instance Alternative Error where
   empty = mzero
   (<|>) = mplus

instance Monad Error where
   fail    = Error . Left
   return  = Error . Right
   m >>= f = Error $ either Left (runError . f) (runError m)

instance MonadPlus Error where
   mzero     = fail "mzero"
   mplus p q = Error $
      case (runError p, runError q) of
         (Right a, _) -> Right a
         (_, Right a) -> Right a
         (Left s, _)  -> Left s

runErrorM :: Monad m => Error a -> m a
runErrorM = either fail return . runError