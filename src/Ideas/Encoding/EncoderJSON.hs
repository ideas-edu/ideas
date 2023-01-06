{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed `r the
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

import Control.Applicative hiding (Const)
import Data.Maybe
import Ideas.Common.Library hiding (exerciseId)
import Ideas.Encoding.Encoder
import Ideas.Encoding.Request
import Ideas.Service.State
import Ideas.Service.BasicServices
import Ideas.Service.Types hiding (String)
import Ideas.Text.JSON
import Ideas.Utils.Prelude (distinct)
import qualified Ideas.Service.Diagnose as Diagnose
import qualified Ideas.Service.Apply as Apply
import qualified Ideas.Service.Submit as Submit
import qualified Ideas.Service.Types as Tp

type JSONEncoder a = EncoderX a JSON

jsonEncoder :: TypedEncoder a JSON
jsonEncoder tv@(val ::: tp) =
   case tp of
      _ | length (tupleList tv) > 1 ->
         jsonTuple <$> sequence [ jsonEncoder x | x <- tupleList tv ]
      Iso p t   -> jsonEncoder (to p val ::: t)
      t1 :|: t2 -> case val of
         Left  x -> jsonEncoder (x ::: t1)
         Right y -> jsonEncoder (y ::: t2)
      Pair t1 t2 ->
         let f x y = jsonTuple [x, y]
         in liftA2 f (jsonEncoder (fst val ::: t1))
                     (jsonEncoder (snd val ::: t2))
      List (Const Rule) ->
         return $ Array $ map ruleShortInfo val
      Tp.Tag s t
         | s == "Result"      -> encodeTyped encodeResult Submit.tResult tv
         | s == "Diagnosis"   -> encodeTyped encodeDiagnosis Diagnose.tDiagnosis tv
         | s == "ApplyResult" -> encodeTyped encodeApplyResult Apply.tApplyResult tv
         | s == "Derivation"  -> ((encodeDerivation, tDerivation tStepInfo tContext) <?>
                                 encodeTyped encodeDerivationText (tDerivation tString tContext)) tv
         | s == "first"       -> encodeTyped encodeFirst (tPair tStepInfo tState) (val ::: t)
         | s == "elem"        -> jsonEncoder (val ::: t)
         | s `elem` ["step", "accept", "message"] -> jsonEncoder (val ::: t)
         | otherwise -> (\b -> Object [(s, b)]) <$> jsonEncoder (val ::: t)
      Tp.Unit   -> return Null
      Tp.List t -> Array <$> sequence [ jsonEncoder (x ::: t) | x <- val ]
      Const ctp -> jsonEncodeConst (val ::: ctp)
      _ -> errorStr $ "Cannot encode type: " ++ show tp
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
   tupleList a = [a]

jsonEncodeConst :: TypedValue (Const a) -> JSONEncoder a
jsonEncodeConst (val ::: tp) =
   case tp of
      SomeExercise -> case val of
                         Some ex -> return (exerciseInfo ex)
      State        -> encodeState val
      Rule         -> return (toJSON (showId val))
      Context      -> encodeContext val
      Location     -> return (toJSON (show val))
      Constraint   -> return (toJSON (show val))
      Environment  -> encodeEnvironment val
      Term         -> return (termToJSON val)
      Text         -> return (toJSON (show val))
      Int          -> return (toJSON val)
      Bool         -> return (toJSON val)
      Tp.String    -> return (toJSON val)
      _ -> errorStr $ "Type " ++ show tp ++ " not supported in JSON"

--------------------------

-- legacy representation
encodeEnvironment :: Environment -> JSONEncoder a
encodeEnvironment env = return $
   let f a = Object [(showId a, String (showValue a))]
   in Array [ f a | a <- bindings env ]

encodeContext :: Context a -> JSONEncoder a
encodeContext ctx = f . useJSONTerm <$> getRequest <*> getExercise
 where
   f True  ex = fromMaybe Null $ liftA2 build (hasJSONView ex) $ fromContext ctx
   f False ex = String $ prettyPrinterContext ex ctx

encodeState :: State a -> JSONEncoder a
encodeState st =
   let ctx   = stateContext st
       get f = String (fromMaybe "" (f st))
       make pp env = Array $
          [ String $ showId (exercise st)
          , String $ if withoutPrefix st
                     then "no prefix"
                     else show (statePrefix st)
          , pp
          , env
          ] ++ if isNothing (stateUser st) then [] else
          [ Array [get stateUser, get stateSession, get stateStartTerm] ]
   in make <$> encodeContext ctx <*> encodeStateEnvironment ctx

encodeStateEnvironment :: Context a -> JSONEncoder a
encodeStateEnvironment ctx = return $
   let loc = fromLocation (location ctx)
       env = (if null loc then id else insertRef (makeRef "location") loc)
           $ environment ctx
   in Object [ (showId a, String $ showValue a) | a <- bindings env ]

encodeFirst :: (StepInfo a, State a) -> JSONEncoder a
encodeFirst (step, state) = do
   x <- jsonEncoder (step  ::: tStepInfo)
   y <- jsonEncoder (state ::: tState)
   case x of
      Array xs -> return $ Array $ xs ++ [y]
      _        -> return $ Array [x, y]

encodeDerivation :: Derivation (StepInfo a) (Context a) -> JSONEncoder a
encodeDerivation d =
   let xs = [ ((r, env), ctx) | (_, (r, _, env), ctx) <- triples d ]
   in jsonEncoder (xs ::: tList (tPair (tPair tRule tEnvironment) tContext))

encodeDerivationText :: Derivation String (Context a) -> JSONEncoder a
encodeDerivationText d =
   let xs = [ (s, a) | (_, s, a) <- triples d ]
   in jsonEncoder (xs ::: tList (tPair tString tContext))

encodeResult :: Submit.Result a -> JSONEncoder a
encodeResult result = Object <$>
   case result of
      Submit.Buggy rs -> return
         [ ("result", String "Buggy")
         , ("rules", Array $ map (String . showId) rs)
         ]
      Submit.NotEquivalent s -> return $
         ("result", String "NotEquivalent") :
         [ ("reason", String s) | not (null s)]
      Submit.Ok rs st ->
         let f x =
                [ ("result", String "Ok")
                , ("rules", Array $ map (String . showId) rs)
                , ("state", x)
                ]
         in f <$> jsonEncoder (st ::: tState)
      Submit.Detour rs st ->
         let f x =
                [ ("result", String "Detour")
                , ("rules", Array $ map (String . showId) rs)
                , ("state", x)
                ]
         in f <$> jsonEncoder (st ::: tState)
      Submit.Unknown st ->
         let f x =
                [ ("result", String "Unknown")
                , ("state", x)
                ]
         in f <$> jsonEncoder (st ::: tState)

encodeDiagnosis :: Diagnose.Diagnosis a -> JSONEncoder a
encodeDiagnosis diagnosis =
   case diagnosis of
      Diagnose.SyntaxError s ->
         return $ Object [("syntaxerror", String s)]
      Diagnose.NotEquivalent s ->
         if null s then return (Object [("notequiv", Null)])
                   else make "notequiv" [fromReason s]
      Diagnose.Buggy env r ->
         make "buggy" [fromEnv env, fromRule r]
      Diagnose.Similar b st mr ->
         make "similar" [fromReady b, fromState st, fromMaybeRule mr]
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
   make s = fmap (\xs -> Object [(s, Array xs)]) . sequence
   fromEnv env      = jsonEncoder (env ::: tEnvironment)
   fromRule r       = return (toJSON (showId r))
   fromMaybeRule mr = return (maybe Null (toJSON . showId) mr)
   fromReady b      = return (Object [("ready", toJSON b)])
   fromState st     = jsonEncoder (st ::: tState)
   fromReason s     = return (Object [("reason", toJSON s)])

-- legacy encoder
encodeApplyResult :: Apply.ApplyResult a -> JSONEncoder a
encodeApplyResult result = 
    jsonEncoder (f result ::: tError tState)
 where
   f :: Apply.ApplyResult a -> Either String (State a)   
   f (Apply.SyntaxError msg) = Left $ "Syntax error: " ++ msg
   f (Apply.Correct _ st)    = Right st
   f (Apply.Buggy _ r)       = Left $ "Buggy rule: " ++ show r
   f Apply.Incorrect         = Left $ "Cannot apply rule"


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

exerciseInfo :: Exercise a -> JSON
exerciseInfo ex = Object
   [ ("exerciseid", toJSON (showId ex))
   , ("description", toJSON (description ex))
   , ("status", toJSON (show (status ex)))
   ]