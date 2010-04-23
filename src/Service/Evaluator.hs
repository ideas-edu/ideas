{-# LANGUAGE GADTs, Rank2Types #-}
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
module Service.Evaluator where

import Common.Context (fromContext)
import Common.Exercise (Exercise)
import Common.Transformation (Rule, name)
import Control.Arrow
import Control.Monad
import Data.Maybe
import Service.ExercisePackage (ExercisePackage, exercise, getExerciseText)
import Service.Definitions
import Service.DomainReasoner

evalService :: Evaluator inp out a -> Service -> inp -> DomainReasoner out
evalService f = eval f . serviceFunction

data Evaluator inp out a = Evaluator 
   { encoder :: Encoder out a
   , decoder :: Decoder inp a
   }

data Encoder s a = Encoder 
   { encodeType  :: forall t . Type a t -> t -> DomainReasoner s
   , encodeTerm  :: a -> DomainReasoner s
   , encodeTuple :: [s] -> s
   }

data Decoder s a = Decoder 
   { decodeType     :: forall t . Type a t -> s -> DomainReasoner (t, s)
   , decodeTerm     :: s -> DomainReasoner a
   , decoderPackage :: ExercisePackage a
   } 

decoderExercise :: Decoder s a -> Exercise a
decoderExercise = exercise . decoderPackage

eval :: Evaluator inp out a -> TypedValue a -> inp -> DomainReasoner out
eval f (tv ::: tp) s = 
   case tp of 
      t1 :-> t2 -> do
         (a, s1) <- decodeType (decoder f) t1 s
         eval f (tv a ::: t2) s1
      _ ->
         encodeType (encoder f) tp tv

decodeDefault :: Decoder s a -> Type a t -> s -> DomainReasoner (t, s)
decodeDefault dec tp s =
   case tp of
      Iso f _ t  -> liftM (first f) (decodeType dec t s)
      Pair t1 t2 -> do
         (a, s1) <- decodeType dec t1 s
         (b, s2) <- decodeType dec t2 s1
         return ((a, b), s2)
      Tag _ t1 ->
         decodeType dec t1 s
      Optional a t1 -> 
         decodeType dec t1 s `mplus` return (a, s)
      Maybe t1 -> 
         liftM (first Just) (decodeType dec t1 s) `mplus` return (Nothing, s)
      Error t -> 
         liftM (first Right) (decodeType dec t s)
      Exercise -> do
         return (exercise (decoderPackage dec), s)
      ExerciseText -> do
         exText <- case getExerciseText (decoderPackage dec) of 
                      Just a  -> return a
                      Nothing -> fail "No support for exercise texts"
         return (exText, s)
      _ ->
         fail $ "No support for argument type: " ++ show tp

encodeDefault :: Encoder s a -> Type a t -> t -> DomainReasoner s
encodeDefault enc tp tv =
   case tp of
      Iso _ f t  -> encodeType enc t (f tv)
      Pair t1 t2 -> do
         let (a, b) = tv
         x <- encodeType enc t1 a
         y <- encodeType enc t2 b
         return (encodeTuple enc [x, y])
      Tag _ t1      -> encodeType enc t1 tv
      Elem t1       -> encodeType enc t1 tv
      Optional _ t1 -> encodeType enc t1 tv
      Maybe t1      -> case tv of
                          Just a  -> encodeType enc t1 a
                          Nothing -> return (encodeTuple enc [])
      Error t       -> either fail (encodeType enc t) tv
      IO t1         -> liftIO tv >>= encodeType enc t1
      Rule          -> encodeType enc String (name tv)
      Term          -> encodeTerm enc tv
      Context       -> fromContext tv >>= encodeType enc Term
      Location      -> encodeType enc String (show tv)
      _             -> fail ("No support for result type: " ++ show tp)