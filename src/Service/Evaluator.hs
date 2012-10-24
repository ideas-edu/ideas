{-# LANGUAGE GADTs, Rank2Types #-}
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
module Service.Evaluator where

import Common.Library
import Control.Monad
import Service.DomainReasoner
import Service.Types

evalService :: Evaluator inp out a -> Service -> inp -> DomainReasoner out
evalService f = eval f . serviceFunction

data Evaluator inp out a = Evaluator
   { encoder :: Encoder out a
   , decoder :: Decoder inp a
   }

type Encoder out a = TypedValue a -> DomainReasoner out

data Decoder s a = Decoder
   { decodeType      :: forall t . Type a t -> s -> DomainReasoner (t, s)
   , decodeTerm      :: s -> DomainReasoner a
   , decoderExercise :: Exercise a
   }

eval :: Evaluator inp out a -> TypedValue a -> inp -> DomainReasoner out
eval f tv@(val ::: tp) s =
   case tp of
      t1 :-> t2 -> do
         (a, s1) <- decodeType (decoder f) t1 s
         eval f (val a ::: t2) s1
      _ ->
         encoder f tv

decodeDefault :: Decoder s a -> Type a t -> s -> DomainReasoner (t, s)
decodeDefault dec tp s =
   case tp of
      Iso p t  -> liftM (from (first p)) (decodeType dec t s)
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
      Exercise ->
         return (decoderExercise dec, s)
      Script -> do
         script <- defaultScript (getId (decoderExercise dec))
         return (script, s)
      _ ->
         fail $ "No support for argument type: " ++ show tp

runIO :: IO a -> IO (Either String a)
runIO m = liftM Right m `catch` (return . Left . show)