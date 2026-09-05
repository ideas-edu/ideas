{-# LANGUAGE OverloadedStrings, GADTs #-}
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
-- Services using XML notation
--
-----------------------------------------------------------------------------

module Ideas.Encoding.EncoderXML
   ( XMLEncoder
   , xmlEncoder, encodeState
   ) where

import Data.Char
import Data.List
import Data.Maybe
import Ideas.Common.Library hiding (exerciseId, tFirst)
import Ideas.Encoding.Encoder
import Ideas.Encoding.OpenMathSupport
import Ideas.Encoding.Request hiding (XML)
import Ideas.Encoding.RulesInfo (rulesInfoXML)
import Ideas.Encoding.StrategyInfo
import Ideas.Service.BasicServices (StepInfo, tStepInfo)
import qualified Ideas.Service.Apply as Apply
import Ideas.Service.Diagnose
import Ideas.Service.FeedbackScript.Syntax
import Ideas.Service.State
import Ideas.Service.Types
import Ideas.Text.OpenMath.Object
import Ideas.Text.XML
import Ideas.Utils.Decoding
import Ideas.Utils.Prelude (munless)
import qualified Ideas.Service.FeedbackText as FeedbackText
import qualified Ideas.Service.ProblemDecomposition as PD

-----------------

type XMLEncoder a = EncoderX a XMLBuilder

xmlEncoder :: TypedEncoder a XMLBuilder
xmlEncoder =
   (encodeDiagnosis, tDiagnosis) <?>
   (encodeApplyResult, Apply.tApplyResult) <?>
   (encodeDecompositionReply, PD.tReply) <?>
   (encodeDerivation, tDerivation tStepInfo tContext) <?>
   (encodeFirsts, tList tFirst) <?>
   (encodeFirst, tFirst) <?>
   (encodeDerivationText, tDerivation tString tContext) <?>
   (encodeDifficulty, tDifficulty) <?>
   (encodeMessage, FeedbackText.tMessage) <?>
   \(val ::: tp) ->
   case tp of
      -- meta-information
      Tag "RuleShortInfo" t ->
         case equal t (Const Rule) of
            Just f  -> ruleShortInfo (f val)
            Nothing -> errorStr "rule short info"
      Tag "RulesInfo" _ -> do
         ex    <- getExercise
         useOM <- useOpenMath <$> getRequest
         return (rulesInfoXML ex (buildExpression useOM ex))
      Tag "elem" t ->
         tag "elem" (xmlEncoder (val ::: t))
      -- special cases for lists
      List (Const Rule) ->
         encodeAsList [ ruleShortInfo r | r <- val ]
      List t ->
         encodeAsList [ xmlEncoder (a ::: t) | a <- val ]
      -- standard
      Tag _ t    -> xmlEncoder (val ::: t)
      Iso iso t  -> xmlEncoder (to iso val ::: t)
      Pair t1 t2 -> xmlEncoder (fst val ::: t1) <>
                    xmlEncoder (snd val ::: t2)
      t1 :|: t2  -> case val of
                       Left  a -> xmlEncoder (a ::: t1)
                       Right b -> xmlEncoder (b ::: t2)
      Unit       -> mempty
      Const t    -> xmlEncoderConst (val ::: t)
      _ -> errorStr $ show tp

xmlEncoderConst :: TypedValue (Const a) -> XMLEncoder a
xmlEncoderConst tv@(val ::: tp) =
   case tp of
      SomeExercise -> case val of
                         Some a -> exerciseInfo a
      Strategy     -> builder (strategyToXML val)
      Rule         -> "ruleid" .=. show val
      Constraint   -> "constraint" .=. show val
      State        -> encodeState val
      Context      -> encodeContext val
      Location     -> encodeLocation val
      Environment  -> encodeEnvironment val
      Term         -> builderXML (toOMOBJ val)
      Text         -> encodeText val
      Bool         -> string (showBool val)
      XML          -> builder val
      _            -> text tv

encodeState :: State a -> XMLEncoder a
encodeState st = element "state"
   [ if withoutPrefix st
     then mempty
     else element "prefix" [string (show (statePrefix st))]
   , encodeContext (stateContext st)
   ]

encodeContext :: Context a -> XMLEncoder a
encodeContext ctx = do
   ex    <- getExercise
   useOM <- useOpenMath <$> getRequest
   maybe (error "encodeContext") (buildExpression useOM ex) (fromContext ctx)
      <>
      let values = bindings (withLoc ctx)
          loc    = fromLocation (location ctx)
          withLoc
             | null loc  = id
             | otherwise = insertRef (makeRef ("location" :: String)) loc
      in munless (null values) $ element "context"
            [  element "item"
                  [ "name"  .=. showId tb
                  , case getTermValue tb of
                       term | useOM ->
                          builder (omobj2xml (toOMOBJ term))
                       _ -> "value" .=. showValue tb
                  ]
            | tb <- values
            ]

buildExpression :: BuildXML b => Bool -> Exercise a -> a -> b
buildExpression useOM ex
   | useOM     = maybe msg builderXML . toOpenMath ex
   | otherwise = tag "expr" . string . prettyPrinter ex
 where
   msg = error "Error encoding term in OpenMath"

encodeLocation :: Location -> XMLEncoder a
encodeLocation loc = "location" .=. show loc

encodeEnvironment :: HasEnvironment env => env -> XMLEncoder a
encodeEnvironment env = mconcat [ encodeTypedBinding b | b <- bindings env ]

encodeTypedBinding :: Binding -> XMLEncoder a
encodeTypedBinding tb = do
   useOM <- useOpenMath <$> getRequest
   tag "argument" $
      ("description" .=. showId tb) <>
      case getTermValue tb of
         term | useOM -> builder $
            omobj2xml $ toOMOBJ term
         _ -> string (showValue tb)

encodeDerivation :: Derivation (StepInfo a) (Context a) -> XMLEncoder a
encodeDerivation d =
   let xs = [ ((r, env), a) | (_, (r, _, env), a) <- triples d ]
   in xmlEncoder (xs ::: tList (tPair (tPair tRule tEnvironment) tContext))

encodeDerivationText :: Derivation String (Context a) -> XMLEncoder a
encodeDerivationText d = encodeAsList
   [ ("ruletext" .=. s) <> encodeContext a
   | (_, s, a) <- triples d
   ]

tFirst :: Type a (StepInfo a, State a)
tFirst = Tag "first" (tPair tStepInfo tState)

encodeFirst :: (StepInfo a, State a) -> XMLEncoder a
encodeFirst (step, st) = 
   tag "elem" (xmlEncoder (step ::: tStepInfo) <> encodeState st)

encodeFirsts :: [(StepInfo a, State a)] -> XMLEncoder a
encodeFirsts = 
   element "list" . map encodeFirst

ruleShortInfo :: Rule (Context a) -> XMLEncoder a
ruleShortInfo r = mconcat
   [ "name"        .=. showId r
   , "buggy"       .=. showBool (isBuggy r)
   , "arguments"   .=. show (length (getRefs r))
   , "rewriterule" .=. showBool (isRewriteRule r)
   ]

encodeDifficulty :: Difficulty -> XMLEncoder a
encodeDifficulty d =
   "difficulty" .=. show d

encodeText :: Text -> XMLEncoder a
encodeText txt = do
   ex    <- getExercise
   useOM <- useOpenMath <$> getRequest
   mconcat (intersperse (string " ") [ encodeItem ex useOM item | item <- textItems txt ])
 where
   encodeItem ex useOM item =
      case item of
         TextTerm a -> fromMaybe (text item) $ do
            v <- hasTermView ex
            b <- match v a
            return (buildExpression useOM ex b)
         _ -> text item

encodeMessage :: FeedbackText.Message -> XMLEncoder a
encodeMessage msg =
   element "message"
      [ case FeedbackText.accept msg of
           Just b  -> "accept" .=. showBool b
           Nothing -> mempty
      , encodeText (FeedbackText.text msg)
      ]

encodeApplyResult :: Apply.ApplyResult a -> XMLEncoder a
encodeApplyResult result = 
   case result of 
      Apply.SyntaxError msg -> "error" .=. msg
      Apply.Correct _ st    -> encodeState st
      Apply.Buggy _ r       -> "ruleid" .=. show r
      Apply.Incorrect       -> "error" .=. "incorrect"

encodeDiagnosis :: Diagnosis a -> XMLEncoder a
encodeDiagnosis diagnosis =
   case diagnosis of
      SyntaxError s -> element "syntaxerror" [string s]
      Buggy env r -> element "buggy"
         [encodeEnvironment env, "ruleid" .=. showId r]
      NotEquivalent s ->
          if null s then emptyTag "notequiv"
                    else element "notequiv" [ "reason" .=.  s ]
      Similar b st mr -> element "similar" $
         ["ready" .=. showBool b, encodeState st] ++
         maybe [] (\r -> ["ruleid" .=. showId r]) mr
      WrongRule b st mr -> element "wrongrule" $
         [ "ready" .=. showBool b, encodeState st ] ++
         maybe [] (\r -> ["ruleid" .=. showId r]) mr
      Expected b st r -> element "expected"
         ["ready" .=. showBool b, encodeState st, "ruleid" .=. showId r]
      Detour b st env r -> element "detour"
         [ "ready" .=. showBool b, encodeState st
         , encodeEnvironment env, "ruleid" .=. showId r
         ]
      Correct b st -> element "correct"
         ["ready" .=. showBool b, encodeState st]
      Unknown b st -> element "unknown"
         ["ready" .=. showBool b, encodeState st]

encodeDecompositionReply :: PD.Reply a -> XMLEncoder a
encodeDecompositionReply reply =
   case reply of
      PD.Ok loc st ->
         element "correct" [encLoc loc, encodeState st]
      PD.Incorrect eq loc st env ->
         element "incorrect"
            [ "equivalent" .=. showBool eq
            , encLoc loc
            , encodeState st
            , encodeEnvironment env
            ]
 where
    encLoc = tag "location" . text

exerciseInfo :: Exercise b -> XMLEncoder a
exerciseInfo ex = mconcat
   [ "exerciseid"  .=. showId ex
   , "description" .=. description ex
   , "status"      .=. show (status ex)
   ]

------------------------------------------------
-- helpers

encodeAsList :: [XMLEncoder a] -> XMLEncoder a
encodeAsList = element "list" . map (tag "elem")

showBool :: Bool -> String
showBool = map toLower . show