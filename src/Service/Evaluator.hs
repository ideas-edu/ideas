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

import Common.Library
import Control.Monad
import Service.ExercisePackage
import Service.Types
import Service.DomainReasoner
import System.Random

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
      t1 :|: t2 ->
         liftM (first Left)  (decodeType dec t1 s) `mplus`
         liftM (first Right) (decodeType dec t2 s)
      Unit -> 
         return ((), s)
      Tag _ t1 ->
         decodeType dec t1 s
      ExercisePkg ->
         return (decoderPackage dec, s)
      StdGen -> do
         stdgen <- liftIO newStdGen
         return (stdgen, s)
      _ ->
         fail $ "No support for argument type: " ++ show tp

encodeDefault :: Encoder s a -> Type a t -> t -> DomainReasoner s
encodeDefault enc tp tv =
   case tp of
      Iso _ f t  -> rec t (f tv)
      Pair t1 t2 -> do
         let (a, b) = tv
         x <- rec t1 a
         y <- rec t2 b
         return (encodeTuple enc [x, y])
      List t        -> liftM (encodeTuple enc) (mapM (rec t) tv)
      t1 :|: t2     -> case tv of
                          Left  a -> rec t1 a
                          Right b -> rec t2 b
      Unit          -> return (encodeTuple enc [])
      Tag _ t1      -> rec t1 tv
      Rule          -> rec String (showId tv)
      Term          -> encodeTerm enc tv
      Context       -> fromContext tv >>= rec Term
      Location      -> recShow tv
      Id            -> recShow tv
      Int           -> recShow tv
      ExercisePkg   -> return (encodeTuple enc [])
      Exception     -> fail tv
      _             -> fail ("No support for result type: " ++ show tp)
 where
   rec = encodeType enc
   recShow a = rec String (show a)