{-# LANGUAGE GADTs #-}
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
-- Services using JSON notation
--
-----------------------------------------------------------------------------

module Ideas.Encoding.EncoderJSON (jsonEncoder) where

import Data.Maybe
import Ideas.Common.Library hiding (exerciseId)
import Ideas.Common.Utils (Some(..), distinct)
import Ideas.Encoding.Encoder
import Ideas.Service.State
import Ideas.Service.Types hiding (String)
import Ideas.Text.JSON
import qualified Ideas.Service.Diagnose as Diagnose
import qualified Ideas.Service.Submit as Submit
import qualified Ideas.Service.Types as Tp

type JSONEncoder a t = Encoder a t JSON

jsonEncoder :: TypedEncoder a JSON
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
      | s `elem` ["Message"] = tupleList (x ::: t)
   tupleList (ev ::: (t1 :|: t2)) =
      either (\x -> tupleList (x ::: t1))
             (\x -> tupleList (x ::: t2)) ev
   tupleList tv = [tv]

jsonEncodeConst :: JSONEncoder a (TypedValue (Const a))
jsonEncodeConst = encoderFor $ \(val ::: tp) ->
   case tp of
      SomeExercise -> case val of
                         Some ex -> pure (exerciseInfo ex)
      State        -> encodeState // val
      Rule         -> pure (toJSON (showId val))
      Context      -> encodeContext // val
      Location     -> pure (toJSON (show val))
      Environment  -> encodeEnvironment // val
      Text         -> pure (toJSON (show val))
      Int          -> pure (toJSON val)
      Bool         -> pure (toJSON val)
      Tp.String    -> pure (toJSON val)
      _ -> fail $ "Type " ++ show tp ++ " not supported in JSON"

--------------------------

-- legacy representation
encodeEnvironment :: JSONEncoder a Environment
encodeEnvironment = makeEncoder $ \env ->
   let f a = Object [(showId a, String (showValue a))]
   in Array [ f a | a <- bindings env ]

encodeContext :: JSONEncoder a (Context a)
encodeContext = exerciseEncoder $ \ex ctx ->
   String $ prettyPrinterContext ex ctx

encodeState :: JSONEncoder a (State a)
encodeState = encoderFor $ \st ->
   let ctx   = stateContext st
       get f = String (fromMaybe "" (f st))
       make pp env = Array $
          [ String $ showId (exercise st)
          , String $ if withoutPrefix st
                     then "NoPrefix"
                     else show (statePrefix st)
          , pp
          , env
          ] ++ if isNothing (stateUser st) then [] else
          [ Array [get stateUser, get stateSession, get stateStartTerm] ]
   in make <$> (encodeContext // ctx) <*> (encodeStateEnvironment // ctx)

encodeStateEnvironment :: JSONEncoder a (Context a)
encodeStateEnvironment = makeEncoder $ \ctx ->
   let loc = fromLocation (location ctx)
       env = (if null loc then id else insertRef (makeRef "location") loc)
           $ environment ctx
   in Object [ (showId a, String $ showValue a) | a <- bindings env ]

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
   make s = liftA (\xs -> Object [(s, Array xs)]) . sequence
   fromEnv env      = jsonEncoder // (env ::: tEnvironment)
   fromRule r       = pure (toJSON (showId r))
   fromMaybeRule mr = pure (maybe Null (toJSON . showId) mr)
   fromReady b      = pure (Object [("ready", toJSON b)])
   fromState st     = jsonEncoder // (st ::: tState)
   fromReason s     = pure (Object [("reason", toJSON s)])

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
   case mapM f xs of
      Just ys | distinct (map fst ys) -> Object ys
      _ -> Array xs
 where
   f (Object [p]) = Just p
   f _ = Nothing

ruleShortInfo :: Rule a -> JSON
ruleShortInfo r = Object
   [ ("name",        toJSON (showId r))
   , ("buggy",       toJSON (isBuggy r))
   , ("arguments",   toJSON (length (getRefs r)))
   , ("rewriterule", toJSON (isRewriteRule r))
   ]

exerciseInfo :: Exercise a -> JSON
exerciseInfo ex = Object
   [ ("exerciseid", toJSON (showId ex))
   , ("description", toJSON (description ex))
   , ("status", toJSON (show (status ex)))
   ]