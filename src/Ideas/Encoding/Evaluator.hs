{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
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

module Ideas.Encoding.Evaluator (Evaluator(..), evalService) where

import Ideas.Common.Library
import Ideas.Encoding.Encoder
import Ideas.Encoding.Logging
import Ideas.Encoding.Options
import Ideas.Service.Diagnose
import Ideas.Service.Types
import Ideas.Utils.Decoding

data Evaluator a b c = Evaluator (TypedDecoder a b) (TypedEncoder a c)

data EvalResult a c = EvalResult
   { inputValues :: [TypedValue (Type a)]
   , outputValue :: TypedValue (Type a)
   , evalResult  :: c
   }

values :: EvalResult a c -> [TypedValue (Type a)]
values result = outputValue result : inputValues result

logType :: Options -> EvalResult a c -> Type a b -> (b -> Record -> Record) -> IO ()
logType opts res tp f =
   case concatMap (findValuesOfType tp) (values res) of
      []   -> return ()
      hd:_ -> changeLog (logRef opts) (f hd)

evalService :: Exercise a -> Options -> Evaluator a b c -> Service -> b -> IO c
evalService ex opts f srv b = do
   res <- eval ex opts f b (serviceFunction srv)
   logType opts res tState addState
   logType opts res tRule $ \rl r -> r {ruleid = showId rl}
   logType opts res tDiagnosis $ \d r -> r {serviceinfo = show d}
   return (evalResult res)

eval :: forall a b c. Exercise a -> Options -> Evaluator a b c -> b -> TypedValue (Type a) -> IO (EvalResult a c)
eval ex opts (Evaluator dec enc) b = rec
 where
   env = (ex, opts)

   rec :: TypedValue (Type a) -> IO (EvalResult a c)
   rec tv@(val ::: tp)  = 
      case tp of
         -- handle exceptions
         Const String :|: t ->
            either fail (\a -> rec (a ::: t)) val
         -- uncurry function if possible
         t1 :-> t2 :-> t3 ->
            rec (uncurry val ::: Pair t1 t2 :-> t3)
         -- functions
         t1 :-> t2 ->
            case evalDecoder (dec t1) env b of
               Left msg -> fail msg
               Right a -> do 
                  res <- rec (val a ::: t2)
                  return res { inputValues = (a ::: t1) : inputValues res }
         -- perform IO
         IO t -> do
            a <- val
            rec (a ::: t)
         _ ->
            either fail (return . EvalResult [] tv) (runEncoder (enc tv) env)