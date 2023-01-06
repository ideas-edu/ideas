{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- Copyright 2018, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Services using JSON notation
--
-----------------------------------------------------------------------------

module Ideas.Encoding.NewEncoderJSON (jsonEncoder) where

import Data.Char
import Ideas.Common.Library hiding (exerciseId)
import Ideas.Encoding.Encoder
import Ideas.Service.State
import Ideas.Service.Types hiding (String)
import Ideas.Text.JSON
import qualified Ideas.Service.Apply as Apply
import qualified Ideas.Service.Diagnose as Diagnose
import qualified Ideas.Service.Types as Tp

-------------------------------------------------------------

jsonEncoder :: TypedValue (Type a) -> EncoderX a JSONBuilder
jsonEncoder tv@(val ::: tp) = 
   case tp of
      Iso p t    -> jsonEncoder (to p val ::: t)
      t1 :|: t2  -> case val of
                       Left  x -> jsonEncoder (x ::: t1)
                       Right y -> jsonEncoder (y ::: t2)
      Pair t1 t2 -> (<>) <$> jsonEncoder (fst val ::: t1) <*> jsonEncoder (snd val ::: t2)
      Tp.List t  -> arrayBuilder <$> sequence [ jsonEncoder (x ::: t) | x <- val ]
      Tp.Tag s t 
         | s == "Diagnosis"   -> encodeTyped encodeDiagnosis Diagnose.tDiagnosis tv
         | s == "ApplyResult" -> encodeTyped encodeApplyResult Apply.tApplyResult tv
         | otherwise          -> (map toLower s .=) <$> jsonEncoder (val ::: t)
      Tp.Unit    -> pure mempty
      Const ctp  -> jsonEncodeConst (val ::: ctp)
      _          -> errorStr $ "Cannot encode type: " ++ show tp

jsonEncodeConst :: TypedValue (Const a) -> EncoderX a JSONBuilder
jsonEncodeConst (val ::: tp) = 
   case tp of
      Rule         -> encodeRule val
      Constraint   -> encodeConstraint val
      Location     -> encodeLocation val
      Environment  -> encodeEnvironment val
      Context      -> encodeContext val
      State        -> encodeState val
      SomeExercise -> case val of
                         Some ex -> pure (exerciseInfo ex)
      Text         -> pure $ jsonBuilder (show val)
      Tp.String    -> pure $ jsonBuilder val
      Tp.Int       -> pure $ jsonBuilder val
      Tp.Bool      -> pure $ jsonBuilder val
      _ -> errorStr $ "Type " ++ show tp ++ " not supported in JSON"

encodeRule :: Rule (Context a) -> EncoderX a JSONBuilder
encodeRule r = pure $ "rule" .= showId r

encodeConstraint :: Constraint (Context a) -> EncoderX a JSONBuilder
encodeConstraint c = pure $ "constraint" .= showId c

encodeEnvironment :: Environment -> EncoderX a JSONBuilder
encodeEnvironment env =
   let f a = (showId a, String (showValue a))
   in pure $ "environment" .= Object [ f a | a <- bindings env ]

encodeContext :: Context a -> EncoderX a JSONBuilder
encodeContext ctx = 
   let encValue = case fromContext ctx of
                     Just a  -> encodeTerm a
                     Nothing -> pure $ "term" .= Null -- todo: merge with encodeTerm
       encEnv = encodeEnvironment (environment ctx)
       encLoc = encodeLocation (location ctx)
   in (\xs ys zs -> "context" .= (xs <> ys <> zs)) <$> encValue <*> encEnv <*> encLoc

encodeTerm :: a -> EncoderX a JSONBuilder
encodeTerm a = tagJSON "term" . f <$> getExercise
 where
   f ex = 
      case hasJSONView ex of
         Just jv -> build jv a
         Nothing -> String (prettyPrinter ex a)

encodeLocation :: Location -> EncoderX a JSONBuilder
encodeLocation loc = pure $ "location" .= fromLocation loc

encodeState :: State a -> EncoderX a JSONBuilder
encodeState st =
   let ctx   = stateContext st
       get f = maybe Null String (f st)
       make ppCtx = mconcat
          [ "exerciseid" .= showId (exercise st)
          , "prefix"     .= if withoutPrefix st
                            then Null
                            else String (show (statePrefix st))
          , ppCtx
          , "userid"    .= get stateUser
          , "sessionid" .= get stateSession
          , "taskid"    .= get stateStartTerm
          ]
   in ("state" .=) . make <$> encodeContext ctx

encodeDiagnosis :: Diagnose.Diagnosis a -> EncoderX a JSONBuilder
encodeDiagnosis diagnosis =
   case diagnosis of
      Diagnose.Correct b st ->
         (\xs ys -> "diagnosetype" .= "correct" <> xs <> ys) <$> mkReady b <*> encodeState st 
      Diagnose.Similar b st mr ->
         (\xs ys zs -> "diagnosetype" .= "similar" <> xs <> ys <> zs) <$> mkReady b <*> encodeState st <*> mkMaybeRule mr
      Diagnose.NotEquivalent s ->
         pure $ "diagnosetype" .= "notequiv" <> "message" .= s
      Diagnose.Expected b st r ->
         (\xs ys zs -> "diagnosetype" .= "expected" <> xs <> ys <> zs) <$> mkReady b <*> encodeState st <*> mkRule r
      Diagnose.Buggy env r ->
         (\xs ys -> "diagnosetype" .= "buggy" <> xs <> ys) <$> encodeEnvironment env <*> mkRule r
      Diagnose.Detour b st env r ->
          (\xs ys zs vs -> "diagnosetype" .= "detour" <> xs <> ys <> zs <> vs) <$> mkReady b <*> encodeState st <*> encodeEnvironment env <*> mkRule r
      Diagnose.WrongRule b st mr ->
         (\xs ys zs -> "diagnosetype" .= "wrongrule" <> xs <> ys <> zs) <$> mkReady b <*> encodeState st <*> mkMaybeRule mr
      Diagnose.SyntaxError msg -> 
          pure $ "diagnosetype" .= "syntaxerror" <> "message" .= msg
      Diagnose.Unknown b st ->
         (\xs ys -> "diagnosetype" .= "unknown" <> xs <> ys) <$> mkReady b <*> encodeState st 
 where
  mkReady b      = pure $ "ready" .= b
  mkRule         = mkMaybeRule . Just
  mkMaybeRule mr = pure $ "rule" .= maybe Null (toJSON . showId) mr

encodeApplyResult :: Apply.ApplyResult a -> EncoderX a JSONBuilder
encodeApplyResult result = 
   case result of
      Apply.Correct b st ->
         (\xs ys -> "diagnosetype" .= "correct" <> xs <> ys) <$> mkReady b <*> encodeState st 
      Apply.SyntaxError msg -> 
         pure $ "diagnosetype" .= "syntaxerror" <> "message" .= msg
      Apply.Buggy env r ->
         (\xs ys -> "diagnosetype" .= "buggy" <> xs <> ys) <$> encodeEnvironment env <*> mkRule r
      Apply.Incorrect ->
         pure $ "diagnosetype" .= "incorrect"

 where
  mkReady b      = pure $ "ready" .= b
  mkRule         = mkMaybeRule . Just
  mkMaybeRule mr = pure $ "rule" .= maybe Null (toJSON . showId) mr

exerciseInfo :: Exercise a -> JSONBuilder
exerciseInfo ex = "exercise" .= mconcat
   [ "exerciseid"  .= showId ex
   , "description" .= description ex
   , "status"      .= show (status ex)
   ]