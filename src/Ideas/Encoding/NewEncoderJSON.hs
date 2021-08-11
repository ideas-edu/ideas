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

module Ideas.Encoding.NewEncoderJSON (JSONBuilder, builderToJSON, jsonEncoder) where

import Data.Char
import Ideas.Common.Library hiding (exerciseId)
import Ideas.Encoding.Encoder
import Ideas.Service.State
import Ideas.Service.Types hiding (String)
import Ideas.Text.JSON
import qualified Ideas.Service.Diagnose as Diagnose
import qualified Ideas.Service.Types as Tp

type JSONBuilder = [(Maybe Key, JSON)]

listJSONBuilder :: [JSONBuilder] -> JSONBuilder
listJSONBuilder xs = [(Nothing, Array (map builderToJSON xs))]

builderToJSON :: JSONBuilder -> JSON
builderToJSON [(Nothing, x)] = x
builderToJSON xs = 
   case mapM fst xs of
      Just ks -> Object (zip ks (map snd xs))
      Nothing -> Array (map f xs)
 where
    f (Nothing, a) = a
    f (Just k, a)  = Object [(k, a)]

tagJSONBuilder :: String -> JSONBuilder -> JSONBuilder
tagJSONBuilder s = tagJSON (map toLower s) . builderToJSON

tagJSON :: String -> JSON -> JSONBuilder
tagJSON s a = [(Just s, a)] 

-------------------------------------------------------------
--
jsonEncoder :: TypedValue (Type a) -> EncoderX a JSONBuilder
jsonEncoder tv@(val ::: tp) = 
   case tp of
      Iso p t    -> jsonEncoder (to p val ::: t)
      t1 :|: t2  -> case val of
                       Left  x -> jsonEncoder (x ::: t1)
                       Right y -> jsonEncoder (y ::: t2)
      Pair t1 t2 -> (++) <$> jsonEncoder (fst val ::: t1) <*> jsonEncoder (snd val ::: t2)
      Tp.List t  -> listJSONBuilder <$> sequence [ jsonEncoder (x ::: t) | x <- val ]
      Tp.Tag s t 
         | s == "Diagnosis"  -> encodeTyped encodeDiagnosis Diagnose.tDiagnosis tv
         | otherwise         -> tagJSONBuilder (map toLower s) <$> jsonEncoder (val ::: t)
      Tp.Unit    -> pure []
      Const ctp  -> jsonEncodeConst (val ::: ctp)
      _          -> fail $ "Cannot encode type: " ++ show tp

jsonEncodeConst :: TypedValue (Const a) -> EncoderX a JSONBuilder
jsonEncodeConst (val ::: tp) = 
   case tp of
      Rule         -> encodeRule val
      Location     -> encodeLocation val
      Environment  -> encodeEnvironment val
      Context      -> encodeContext val
      State        -> encodeState val
      SomeExercise -> case val of
                         Some ex -> pure (exerciseInfo ex)
      Text         -> pure [(Nothing, toJSON (show val))]
      Tp.String    -> pure [(Nothing, toJSON val)]
      Tp.Int       -> pure [(Nothing, toJSON val)]
      Tp.Bool      -> pure [(Nothing, toJSON val)]
      _ -> fail $ "Type " ++ show tp ++ " not supported in JSON"

encodeRule :: Rule (Context a) -> EncoderX a JSONBuilder
encodeRule r = pure (tagJSON "rule" (toJSON (showId r)))

encodeEnvironment :: Environment -> EncoderX a JSONBuilder
encodeEnvironment env =
   let f a = (showId a, String (showValue a))
   in pure $ tagJSON "environment" $ Object [ f a | a <- bindings env ]

encodeContext :: Context a -> EncoderX a JSONBuilder
encodeContext ctx = 
   let encValue = case fromContext ctx of
                     Just a  -> encodeTerm a
                     Nothing -> pure $ tagJSON "term" Null -- todo: merge with encodeTerm
       encEnv = encodeEnvironment (environment ctx)
       encLoc = encodeLocation (location ctx)
   in (\xs ys zs -> tagJSONBuilder "context" $ xs ++ ys ++ zs) <$> encValue <*> encEnv <*> encLoc

encodeTerm :: a -> EncoderX a JSONBuilder
encodeTerm a = (tagJSON "term" . f) <$> getExercise
 where
   f ex = 
      case hasJSONView ex of
         Just jv -> build jv a
         Nothing -> String (prettyPrinter ex a)

encodeLocation :: Location -> EncoderX a JSONBuilder
encodeLocation loc = pure (tagJSON "location" (toJSON (fromLocation loc)))

encodeState :: State a -> EncoderX a JSONBuilder
encodeState st =
   let ctx   = stateContext st
       get f = maybe Null String (f st)
       make ppCtx =
          [ (Just "exerciseid",  String $ showId (exercise st))
          , (Just "prefix",      if withoutPrefix st
                                 then Null
                                 else String (show (statePrefix st))
            )
          ] ++
          ppCtx ++
          [ (Just "userid",    get stateUser)
          , (Just "sessionid", get stateSession)
          , (Just "taskid",    get stateStartTerm)
          ]
   in (tagJSONBuilder "state" . make) <$> (encodeContext ctx)

encodeDiagnosis :: Diagnose.Diagnosis a -> EncoderX a JSONBuilder
encodeDiagnosis diagnosis =
   case diagnosis of
      Diagnose.Correct b st ->
         (\xs ys -> (Just "diagnosetype", toJSON "correct") : xs ++ ys) <$> mkReady b <*> encodeState st 
      Diagnose.Similar b st mr ->
         (\xs ys zs -> (Just "diagnosetype", toJSON "similar") : xs ++ ys ++ zs) <$> mkReady b <*> encodeState st <*> mkMaybeRule mr
      Diagnose.NotEquivalent s ->
         return [(Just "diagnosetype", toJSON "notequiv"), (Just "message", toJSON s)]
      Diagnose.Expected b st r ->
         (\xs ys zs -> (Just "diagnosetype", toJSON "expected") : xs ++ ys ++ zs) <$> mkReady b <*> encodeState st <*> mkRule r
      Diagnose.Buggy env r ->
         (\xs ys -> (Just "diagnosetype", toJSON "buggy") : xs ++ ys) <$> encodeEnvironment env <*> mkRule r
      Diagnose.Detour b st env r ->
          (\xs ys zs vs -> (Just "diagnosetype", toJSON "detour") : xs ++ ys ++ zs ++ vs) <$> mkReady b <*> encodeState st <*> encodeEnvironment env <*> mkRule r
      Diagnose.WrongRule b st mr ->
         (\xs ys zs -> (Just "diagnosetype", toJSON "wrongrule") : xs ++ ys ++ zs) <$> mkReady b <*> encodeState st <*> mkMaybeRule mr
      Diagnose.SyntaxError msg -> 
          pure [(Just "diagnosetype", toJSON "syntaxerror"), (Just "message", toJSON msg)]
      Diagnose.Unknown b st ->
         (\xs ys -> (Just "diagnosetype", toJSON "unknown") : xs ++ ys) <$> mkReady b <*> encodeState st 
 where
  mkReady b      = return [(Just "ready", toJSON b)]
  mkRule         = mkMaybeRule . Just
  mkMaybeRule mr = return [(Just "rule", maybe Null (toJSON . showId) mr)]

exerciseInfo :: Exercise a -> JSONBuilder
exerciseInfo ex = tagJSON "exercise" $ Object  
   [ ("exerciseid", toJSON (showId ex))
   , ("description", toJSON (description ex))
   , ("status", toJSON (show (status ex)))
   ]