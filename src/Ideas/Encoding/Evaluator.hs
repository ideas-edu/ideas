{-# LANGUAGE GADTs, RankNTypes #-}
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

module Ideas.Encoding.Evaluator (Evaluator(..), evalService) where

import Ideas.Common.Library
import Ideas.Encoding.Encoder
import Ideas.Main.Logging
import Ideas.Service.Diagnose
import Ideas.Service.Types

data Evaluator a b c = Evaluator (TypedDecoder a b) (TypedEncoder a c)

data EvalResult a c = EvalResult
   { inputValues :: [TypedValue (Type a)]
   , outputValue :: TypedValue (Type a)
   , evalResult  :: c
   }

values :: EvalResult a c -> [TypedValue (Type a)]
values result = outputValue result : inputValues result

logType :: LogRef -> EvalResult a c -> Type a b -> (b -> Record -> Record) -> IO ()
logType logRef res tp f =
   case concatMap (findValuesOfType tp) (values res) of
      []   -> return ()
      hd:_ -> changeLog logRef (f hd)

evalService :: LogRef -> Options a -> Evaluator a b c -> Service -> b -> IO c
evalService logRef opts f srv b = do
   res <- eval opts f b (serviceFunction srv)
   logType logRef res tState addState
   logType logRef res tRule $ \rl r -> r {ruleid = showId rl}
   logType logRef res tDiagnosis $ \d r -> r {serviceinfo = show d}
   return (evalResult res)

eval :: Options a -> Evaluator a b c -> b -> TypedValue (Type a) -> IO (EvalResult a c)
eval opts (Evaluator dec enc) b = rec
 where
   rec tv@(val ::: tp) =
      case tp of
         -- handle exceptions
         Const String :|: t ->
            either fail (\a -> rec (a ::: t)) val
         -- uncurry function if possible
         t1 :-> t2 :-> t3 ->
            rec (uncurry val ::: Pair t1 t2 :-> t3)
         t1 :-> t2 -> do
            a   <- run (dec t1) opts b
            res <- rec (val a ::: t2)
            return res { inputValues = (a ::: t1) : inputValues res }
         -- perform IO
         IO t -> do
            a <- val
            rec (a ::: t)
         _ -> do
            c <- run enc opts tv
            return $ EvalResult [] tv c