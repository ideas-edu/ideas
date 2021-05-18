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

import Control.Applicative hiding (Const)
import Data.Char
import Data.Maybe
import Ideas.Common.Library hiding (exerciseId)
import Ideas.Encoding.Encoder hiding (symbol)
import Ideas.Encoding.Request
import Ideas.Service.State
import Ideas.Service.Types hiding (String)
import Ideas.Text.JSON
import Ideas.Utils.Prelude (distinct)
import qualified Ideas.Service.Diagnose as Diagnose
import qualified Ideas.Service.Submit as Submit
import qualified Ideas.Service.Types as Tp

type JSONBuilder = [(Maybe Key, JSON)]

listJSONBuilder :: [JSONBuilder] -> JSONBuilder
listJSONBuilder xs = {-
   case mapM isSingleton xs of
      Just ys -> ys
      Nothing -> -} [(Nothing, Array (map builderToJSON xs))]

isSingleton :: [a] -> Maybe a
isSingleton [x] = Just x
isSingleton _   = Nothing

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

{-
   encoderFor $ \(val ::: tp) ->
   case tp of
     Rule         -> encodeRule // val 
     Location     -> encodeLocation // val
     Environment  -> encodeEnvironment // val
     Context      -> encodeContext // val
     State        -> encodeState // val
     _            -> pure [(Nothing, toJSON (show tp))]

    {-
      SomeExercise -> case val of
                         Some ex -> pure (exerciseInfo ex)
      
      Term         -> pure (termToJSON val)
      Text         -> pure (toJSON (show val))
      Int          -> pure (toJSON val)
      Bool         -> pure (toJSON val)
      Tp.String    -> pure (toJSON val)
      _ -> fail $ "Type " ++ show tp ++ " not supported in JSON"
-}
-}
encodeRule :: Rule (Context a) -> EncoderX a JSONBuilder
encodeRule r = pure (tagJSON "rule" (toJSON (showId r)))

encodeEnvironment :: Environment -> EncoderX a JSONBuilder
encodeEnvironment env =
   let f a = (showId a, String (showValue a))
   in pure $ tagJSON "environment" $ Object [ f a | a <- bindings env ]
{-
{-encodeStateEnvironment :: JSONEncoder a (Context a)
encodeStateEnvironment = makeEncoder $ \ctx ->
   let loc = fromLocation (location ctx)
       env = (if null loc then id else insertRef (makeRef "location") loc)
           $ environment ctx
   in Object [ (showId a, String $ showValue a) | a <- bindings env ]
   -}
-}
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


  -- True  ex = tagJSON "term" . fromMaybe Null . liftA2 build (hasJSONView ex) . return
  -- f False ex = tagJSON "term" . String . prettyPrinter ex

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
         (\xs ys -> (Just "diagnosetype", toJSON "correct") : xs ++ ys) <$> encodeReady b <*> encodeState st 
      Diagnose.Similar b st mr ->
         (\xs ys zs -> (Just "diagnosetype", toJSON "similar") : xs ++ ys ++ zs) <$> encodeReady b <*> encodeState st <*> encodeMaybeRule mr
      Diagnose.NotEquivalent s ->
         return [(Just "diagnosetype", toJSON "notequiv"), (Just "message", toJSON s)]
      Diagnose.Expected b st r ->
         (\xs ys zs -> (Just "diagnosetype", toJSON "expected") : xs ++ ys ++ zs) <$> encodeReady b <*> encodeState st <*> encodeRule r
      Diagnose.Buggy env r ->
         (\xs ys -> (Just "diagnosetype", toJSON "buggy") : xs ++ ys) <$> encodeEnvironment env <*> encodeRule r
      Diagnose.Detour b st env r ->
          (\xs ys zs vs -> (Just "diagnosetype", toJSON "detour") : xs ++ ys ++ zs ++ vs) <$> encodeReady b <*> encodeState st <*> encodeEnvironment env <*> encodeRule r
      Diagnose.WrongRule b st mr ->
         (\xs ys zs -> (Just "diagnosetype", toJSON "wrongrule") : xs ++ ys ++ zs) <$> encodeReady b <*> encodeState st <*> encodeMaybeRule mr
      Diagnose.SyntaxError msg -> 
          pure [(Just "diagnosetype", toJSON "syntaxerror"), (Just "message", toJSON msg)]
 where
  encodeReady b      = return [(Just "ready", toJSON b)]
  encodeRule r       = return [(Just "rule", toJSON (showId r))]
  encodeMaybeRule mr = return [(Just "rule", maybe Null (toJSON . showId) mr)]

         -- make "correct" [fromReady b, fromState st]
      
    {- 
      Diagnose.SyntaxError s ->
         pure $ Object [("syntaxerror", String s)]
      Diagnose.NotEquivalent s ->
         if null s then pure (Object [("notequiv", Null)])
                   else make "notequiv" [fromReason s]
      Diagnose.Buggy env r ->
         make "buggy" [fromEnv env, fromRule r]
      Diagnose.Similar b st ->
         make "similar" [fromReady b, fromState st]
      Diagnose.WrongRule b st mr ->
         make "wrongrule" [fromReady b, fromState st, fromMaybeRule mr]
      Diagnose.Expected b st r ->
         make "expected" [fromReady b, fromState st, fromRule r]
      Diagnose.Detour b st env r ->
         make "detour" [fromReady b, fromState st, fromEnv env, fromRule r]
      Diagnose.Correct b st ->
         make "correct" [fromReady b, fromState st]
      Diagnose.Unknown b st ->
         make "unknown" [fromReady b, fromState st]
 where
   make s = fmap (\xs -> Object $ ("diagnosis", toJSON s) : xs) . sequence
   fromEnv env      = fmap (\x -> ("env", x)) $ jsonEncoder // (env ::: tEnvironment)
   fromRule r       = pure ("rule",       (toJSON (showId r)))
   fromMaybeRule mr = pure ("mayberule",  (maybe Null (toJSON . showId) mr))
   fromReady b      = pure ("ready", toJSON b)
   fromState st     = fmap (\x -> ("state", x)) $ jsonEncoder // (st ::: tState)
   fromReason s     = pure ("reason",     (Object [("reason", toJSON s)]))
-}

{-
{-
      Tp.Tag s t
         | s == "Result"     -> encodeTyped encodeResult Submit.tResult
         | s == "Diagnosis"  -> encodeTyped encodeDiagnosis Diagnose.tDiagnosis
         | s == "Derivation" -> (encodeDerivation, tDerivation (tPair tRule tEnvironment) tContext) <?>
                                encodeTyped encodeDerivationText (tDerivation tString tContext)
         | s == "elem"       -> jsonEncoder // (val ::: t)
         | otherwise -> (\b -> Object [(s, b)]) <$> jsonEncoder // (val ::: t)
      Tp.Unit   -> pure Null
      
      Const ctp -> jsonEncodeConst // (val ::: ctp)
      _ -> fail $ "Cannot encode type: " ++ show tp
-}
{-
jsonEncoder = encoderFor $ \tv@(val ::: tp) ->
   case tp of
      _ | length (tupleList tv) > 1 ->
         jsonTuple <$> sequence [ jsonEncoder // x | x <- tupleList tv ]
      Iso p t   -> jsonEncoder // (to p val ::: t)
      t1 :|: t2 -> case val of
         Left  x -> jsonEncoder // (x ::: t1)
         Right y -> jsonEncoder // (y ::: t2)
      Pair t1 t2 ->
         let f x y = jsonTuple [x, y]
         in liftA2 f (jsonEncoder // (fst val ::: t1))
                     (jsonEncoder // (snd val ::: t2))
      List (Const Rule) ->
         pure $ Array $ map ruleShortInfo val
      Tp.Tag s t
         | s == "Result"     -> encodeTyped encodeResult Submit.tResult
         | s == "Diagnosis"  -> encodeTyped encodeDiagnosis Diagnose.tDiagnosis
         | s == "Derivation" -> (encodeDerivation, tDerivation (tPair tRule tEnvironment) tContext) <?>
                                encodeTyped encodeDerivationText (tDerivation tString tContext)
         | s == "elem"       -> jsonEncoder // (val ::: t)
         | otherwise -> (\b -> Object [(s, b)]) <$> jsonEncoder // (val ::: t)
      Tp.Unit   -> pure Null
      Tp.List t -> Array <$> sequence [ jsonEncoder // (x ::: t) | x <- val ]
      Const ctp -> jsonEncodeConst // (val ::: ctp)
      _ -> fail $ "Cannot encode type: " ++ show tp
 where
   tupleList :: TypedValue (TypeRep f) -> [TypedValue (TypeRep f)]
   tupleList (x ::: Tp.Iso p t)   = tupleList (to p x ::: t)
   tupleList (p ::: Tp.Pair t1 t2) =
      tupleList (fst p ::: t1) ++ tupleList (snd p ::: t2)
   tupleList (x ::: Tag s t)
      | s == "Message" = tupleList (x ::: t)
   tupleList (ev ::: (t1 :|: t2)) =
      either (\x -> tupleList (x ::: t1))
             (\x -> tupleList (x ::: t2)) ev
   tupleList tv = [tv]
--------------------------
-- legacy representation
encodeEnvironment :: JSONEncoder a Environment
encodeEnvironment = makeEncoder $ \env ->
   let f a = Object [(showId a, String (showValue a))]
   in Object [("environment", Array [ f a | a <- bindings env ])]
encodeDerivation :: JSONEncoder a (Derivation (Rule (Context a), Environment) (Context a))
encodeDerivation = encoderFor $ \d ->
   let xs = [ (s, a) | (_, s, a) <- triples d ]
   in jsonEncoder // (xs ::: tList (tPair (tPair tRule tEnvironment) tContext))
encodeDerivationText :: JSONEncoder a (Derivation String (Context a))
encodeDerivationText = encoderFor $ \d ->
   let xs = [ (s, a) | (_, s, a) <- triples d ]
   in jsonEncoder // (xs ::: tList (tPair tString tContext))
encodeResult :: JSONEncoder a (Submit.Result a)
encodeResult = encoderFor $ \result -> Object <$>
   case result of
      Submit.Buggy rs -> pure
         [ ("result", String "Buggy")
         , ("rules", Array $ map (String . showId) rs)
         ]
      Submit.NotEquivalent s -> pure $
         ("result", String "NotEquivalent") :
         [ ("reason", String s) | not (null s)]
      Submit.Ok rs st ->
         let f x =
                [ ("result", String "Ok")
                , ("rules", Array $ map (String . showId) rs)
                , ("state", x)
                ]
         in f <$> jsonEncoder // (st ::: tState)
      Submit.Detour rs st ->
         let f x =
                [ ("result", String "Detour")
                , ("rules", Array $ map (String . showId) rs)
                , ("state", x)
                ]
         in f <$> jsonEncoder // (st ::: tState)
      Submit.Unknown st ->
         let f x =
                [ ("result", String "Unknown")
                , ("state", x)
                ]
         in f <$> jsonEncoder // (st ::: tState)
encodeDiagnosis :: JSONEncoder a (Diagnose.Diagnosis a)
encodeDiagnosis = encoderFor $ \diagnosis ->
   case diagnosis of
      Diagnose.SyntaxError s ->
         pure $ Object [("syntaxerror", String s)]
      Diagnose.NotEquivalent s ->
         if null s then pure (Object [("notequiv", Null)])
                   else make "notequiv" [fromReason s]
      Diagnose.Buggy env r ->
         make "buggy" [fromEnv env, fromRule r]
      Diagnose.Similar b st ->
         make "similar" [fromReady b, fromState st]
      Diagnose.WrongRule b st mr ->
         make "wrongrule" [fromReady b, fromState st, fromMaybeRule mr]
      Diagnose.Expected b st r ->
         make "expected" [fromReady b, fromState st, fromRule r]
      Diagnose.Detour b st env r ->
         make "detour" [fromReady b, fromState st, fromEnv env, fromRule r]
      Diagnose.Correct b st ->
         make "correct" [fromReady b, fromState st]
      Diagnose.Unknown b st ->
         make "unknown" [fromReady b, fromState st]
 where
   make s = fmap (\xs -> Object $ ("diagnosis", toJSON s) : xs) . sequence
   fromEnv env      = fmap (\x -> ("env", x)) $ jsonEncoder // (env ::: tEnvironment)
   fromRule r       = pure ("rule",       (toJSON (showId r)))
   fromMaybeRule mr = pure ("mayberule",  (maybe Null (toJSON . showId) mr))
   fromReady b      = pure ("ready", toJSON b)
   fromState st     = fmap (\x -> ("state", x)) $ jsonEncoder // (st ::: tState)
   fromReason s     = pure ("reason",     (Object [("reason", toJSON s)]))
{-
encodeTree :: Tree JSON -> JSON
encodeTree (Node r ts) =
  case r of
    Array [x, t] -> Object
       [ ("rootLabel", x)
       , ("type", t)
       , ("subForest", Array $ map encodeTree ts)
       ]
    _ -> error "ModeJSON: malformed tree!" -}
jsonTuple :: [JSON] -> JSON
jsonTuple xs =
   case catMaybes <$> mapM f xs of
      Just ys | distinct (map fst ys) -> Object ys
      _ -> Array xs
 where
   f (Object [p]) = Just (Just p)
   f Null = Just Nothing
   f _ = Nothing
ruleShortInfo :: Rule a -> JSON
ruleShortInfo r = Object
   [ ("name",        toJSON (showId r))
   , ("buggy",       toJSON (isBuggy r))
   , ("arguments",   toJSON (length (getRefs r)))
   , ("rewriterule", toJSON (isRewriteRule r))
   ]
-}
-}
exerciseInfo :: Exercise a -> JSONBuilder
exerciseInfo ex = tagJSON "exercise" $ Object  
   [ ("exerciseid", toJSON (showId ex))
   , ("description", toJSON (description ex))
   , ("status", toJSON (show (status ex)))
   ]