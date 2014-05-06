{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
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
   ( XMLEncoder, XMLEncoderState(..)
   , xmlEncoder, encodeState
   ) where

import Control.Monad
import Data.Char
import Data.Maybe
import Data.Monoid
import Ideas.Common.Library hiding (exerciseId, (:=), (<|>))
import Ideas.Common.Utils (Some(..))
import Ideas.Encoding.Evaluator
import Ideas.Encoding.OpenMathSupport
import Ideas.Encoding.RulesInfo (rulesInfoXML)
import Ideas.Encoding.StrategyInfo
import Ideas.Service.Diagnose
import Ideas.Service.FeedbackScript.Syntax
import Ideas.Service.State
import Ideas.Service.Types
import Ideas.Text.OpenMath.Object
import Ideas.Text.XML
import qualified Ideas.Service.FeedbackText as FeedbackText
import qualified Ideas.Service.ProblemDecomposition as PD

-----------------

type XMLEncoder a t = EncoderState (XMLEncoderState a) t XMLBuilder

data XMLEncoderState a = XMLEncoderState
   { getExercise :: Exercise a
   , isOpenMath  :: Bool
   , encodeTerm  :: a -> XMLBuilder
   }

xmlEncoder :: XMLEncoder a (TypedValue (Type a))
xmlEncoder = msum
   [ encodeTyped encodeDiagnosis
   , encodeTyped encodeDecompositionReply
   , encodeTyped encodeDerivation
   , encodeTyped encodeDerivationText
   , encodeTyped encodeDifficulty
   , encodeTyped encodeMessage
   , encoderStateFor $ \xp (val ::: tp) ->
        case tp of
           -- meta-information
           Tag "RuleShortInfo" t ->
              case equal t (Const Rule) of
                 Just f  -> ruleShortInfo // f val
                 Nothing -> fail "rule short info"
           Tag "RulesInfo" _ ->
              return (rulesInfoXML (getExercise xp) (encodeTerm xp))
           Tag "elem" t ->
              tag "elem" (xmlEncoder // (val ::: t))
           -- special cases for lists
           List (Const Rule) ->
              encodeAsList [ ruleShortInfo // r | r <- val ]
           List t ->
              encodeAsList [ xmlEncoder // (a ::: t) | a <- val ]
           -- standard
           Tag _ t    -> xmlEncoder // (val ::: t)
           Iso iso t  -> xmlEncoder // (to iso val ::: t)
           Pair t1 t2 -> xmlEncoder // (fst val ::: t1) <>
                         xmlEncoder // (snd val ::: t2)
           t1 :|: t2  -> case val of
                            Left  a -> xmlEncoder // (a ::: t1)
                            Right b -> xmlEncoder // (b ::: t2)
           Unit       -> mempty
           Const t    -> xmlEncoderConst // (val ::: t)
           _ -> fail $ show tp
   ]

xmlEncoderConst :: XMLEncoder a (TypedValue (Const a))
xmlEncoderConst = encoderFor $ \tv@(val ::: tp) ->
   case tp of
      SomeExercise -> case val of
                         Some a -> exerciseInfo // a
      Strategy     -> builder (strategyToXML val)
      Rule         -> "ruleid" .=. show val
      State        -> encodeState // val
      Context      -> encodeContext // val
      Location     -> encodeLocation // val
      Environment  -> encodeEnvironment // val
      Text         -> encodeText // val
      Bool         -> string (showBool val)
      _            -> text tv

encodeState :: XMLEncoder a (State a)
encodeState = encoderFor $ \st -> element "state"
   [ encodePrefixes // statePrefixes st
   , encodeContext // stateContext st
   ]

encodePrefixes :: XMLEncoder a [Prefix (Context a)]
encodePrefixes = encoderFor $ \ps ->
   case ps of
      [] -> mempty
      _  -> element "prefix" $ map (string . showPrefix) ps

encodeContext :: XMLEncoder a (Context a)
encodeContext = encoderStateFor $ \xp ctx ->
   liftM (encodeTerm xp) (fromContext ctx)
   <>
   let values = bindings (withLoc ctx)
       loc    = fromLocation (location ctx)
       withLoc
          | null loc  = id
          | otherwise = insertRef (makeRef "location") loc
   in return $ munless (null values) $ element "context"
         [  element "item"
               [ "name"  .=. showId tb
               , case getTermValue tb of
                    term | isOpenMath xp ->
                       builder (omobj2xml (toOMOBJ term))
                    _ -> "value" .=. showValue tb
               ]
         | tb <- values
         ]

encodeLocation :: XMLEncoder a Location
encodeLocation = encoderFor $ \loc -> return ("location" .=. show loc)

encodeEnvironment :: HasEnvironment env => XMLEncoder a env
encodeEnvironment = encoderFor $ \env ->
   mconcat [ encodeTypedBinding // b | b <- bindings env ]

encodeTypedBinding :: XMLEncoder a Binding
encodeTypedBinding = encoderStateFor $ \xp tb ->
   tag "argument" $
      ("description" .=. showId tb) <>
      case getTermValue tb of
         term | isOpenMath xp -> builder $
            omobj2xml $ toOMOBJ term
         _ -> string (showValue tb)

encodeDerivation :: XMLEncoder a (Derivation (Rule (Context a), Environment) (Context a))
encodeDerivation = encoderFor $ \d ->
   let xs = [ (s, a) | (_, s, a) <- triples d ]
   in xmlEncoder // (xs ::: typed)

encodeDerivationText :: XMLEncoder a (Derivation String (Context a))
encodeDerivationText = encoderFor $ \d -> encodeAsList
   [ ("ruletext" .=. s) <> encodeContext // a
   | (_, s, a) <- triples d
   ]

ruleShortInfo :: XMLEncoder a (Rule (Context a))
ruleShortInfo = simpleEncoder $ \r -> mconcat
   [ "name"        .=. showId r
   , "buggy"       .=. showBool (isBuggy r)
   , "arguments"   .=. show (length (getRefs r))
   , "rewriterule" .=. showBool (isRewriteRule r)
   ]

encodeDifficulty :: XMLEncoder a Difficulty
encodeDifficulty = simpleEncoder $ \d ->
   "difficulty" .=. show d

encodeText :: XMLEncoder a Text
encodeText = encoderFor $ \txt ->
   mconcat [ encodeItem // item | item <- textItems txt ]
 where
   encodeItem = encoderStateFor $ \xp item -> return $
      case item of
         TextTerm a -> fromMaybe (text item) $ do
            v <- hasTermView (getExercise xp)
            b <- match v a
            return (encodeTerm xp b)
         _ -> text item

encodeMessage :: XMLEncoder a FeedbackText.Message
encodeMessage = encoderFor $ \msg ->
   element "message"
      [ case FeedbackText.accept msg of
           Just b  -> "accept" .=. showBool b
           Nothing -> mempty
      , encodeText // FeedbackText.text msg
      ]

encodeDiagnosis :: XMLEncoder a (Diagnosis a)
encodeDiagnosis = encoderFor $ \diagnosis ->
   case diagnosis of
      Buggy env r -> element "buggy"
         [encodeEnvironment // env, "ruleid" .=. showId r]
      NotEquivalent s ->
          if null s then return (emptyTag "notequiv")
                    else element "notequiv" [ "reason" .=.  s ]
      Similar b st -> element "similar"
         ["ready" .=. showBool b, encodeState // st]
      WrongRule b st mr -> element "wrongrule" $
         [ "ready" .=. showBool b, encodeState // st ] ++
         maybe [] (\r -> ["ruleid" .=. showId r]) mr
      Expected b st r -> element "expected"
         ["ready" .=. showBool b, encodeState // st, "ruleid" .=. showId r]
      Detour b st env r -> element "detour"
         [ "ready" .=. showBool b, encodeState // st
         , encodeEnvironment // env, "ruleid" .=. showId r
         ]
      Correct b st -> element "correct"
         ["ready" .=. showBool b, encodeState // st]
      Unknown b st -> element "unknown"
         ["ready" .=. showBool b, encodeState // st]

encodeDecompositionReply :: XMLEncoder a (PD.Reply a)
encodeDecompositionReply = encoderFor $ \reply ->
   case reply of
      PD.Ok loc st ->
         element "correct" [encLoc loc, encodeState // st]
      PD.Incorrect eq loc st env ->
         element "incorrect"
            [ "equivalent" .=. showBool eq
            , encLoc loc
            , encodeState // st
            , encodeEnvironment // env
            ]
 where
    encLoc = tag "location" . text

exerciseInfo :: XMLEncoder a (Exercise b)
exerciseInfo = encoderFor $ \ex -> mconcat
   [ "exerciseid"  .=. showId ex
   , "description" .=. description ex
   , "status"      .=. show (status ex)
   ]

------------------------------------------------
-- helpers

encodeAsList :: [XMLEncoder a t] -> XMLEncoder a t
encodeAsList = element "list" . map (tag "elem")

showBool :: Bool -> String
showBool = map toLower . show