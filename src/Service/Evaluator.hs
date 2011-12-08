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

data Encoder s a = Encoder
   { encodeType    :: forall t . Type a t -> t -> DomainReasoner s
   , encodeCtxTerm :: Context a -> DomainReasoner s
   , encodeTerm    :: a -> DomainReasoner s
   , encodeTuple   :: [s] -> s
   }

data Decoder s a = Decoder
   { decodeType      :: forall t . Type a t -> s -> DomainReasoner (t, s)
   , decodeTerm      :: s -> DomainReasoner a
   , decoderExercise :: Exercise a
   }

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

encodeDefault :: Encoder s a -> Type a t -> t -> DomainReasoner s
encodeDefault enc tp tv =
   case tp of
      Iso p t    -> encodeType enc t (to p tv)
      Pair t1 t2 ->
         case tv of
            (a, b) -> do
               x <- encodeType enc t1 a
               y <- encodeType enc t2 b
               return (encodeTuple enc [x, y])
      List t        -> liftM (encodeTuple enc) (mapM (encodeType enc t) tv)
      t1 :|: t2     -> case tv of
                          Left  a -> encodeType enc t1 a
                          Right b -> encodeType enc t2 b
      Unit          -> return (encodeTuple enc [])
      Tag _ t1      -> encodeType enc t1 tv
      Rule          -> encodeType enc String (showId tv)
      Term          -> encodeTerm enc tv
      Context       -> fromContext tv >>= encodeType enc Term
      Location      -> encodeAsString enc tv
      Id            -> encodeAsString enc tv
      Int           -> encodeAsString enc tv
      Exercise      -> return (encodeTuple enc [])
      IO t          -> do a <- liftIO (runIO tv)
                          encodeType enc (Exception :|: t) a
      Exception     -> fail tv
      _             -> fail ("No support for result type: " ++ show tp)

encodeAsString :: Show b => Encoder s a -> b -> DomainReasoner s
encodeAsString enc a = encodeType enc String (show a)

runIO :: IO a -> IO (Either String a)
runIO m = liftM Right m `catch` (return . Left . show)