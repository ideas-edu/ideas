{-# LANGUAGE GADTs #-}
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
-- Services using XML notation
--
-----------------------------------------------------------------------------
module Ideas.Service.EncoderXML
   ( XMLEncoder, XMLEncoderState(..)
   , xmlEncoder, encodeState
   ) where

import Control.Monad
import Data.Char
import Data.Maybe
import Data.Monoid
import Ideas.Common.Library hiding (exerciseId, (:=), (<|>))
import Ideas.Common.Utils (Some(..))
import Ideas.Service.Diagnose
import Ideas.Service.Evaluator
import Ideas.Service.FeedbackScript.Syntax
import Ideas.Service.OpenMathSupport
import Ideas.Service.RulesInfo (rulesInfoXML)
import Ideas.Service.State
import Ideas.Service.StrategyInfo
import Ideas.Service.BasicServices (StepInfo)
import Ideas.Service.Types
import qualified Ideas.Service.ProblemDecomposition as PD
import Ideas.Text.OpenMath.Object
import Ideas.Text.XML
import qualified Ideas.Service.FeedbackText as FeedbackText

-----------------

type XMLEncoder a t = EncoderState (XMLEncoderState a) t XMLBuilder

data XMLEncoderState a = XMLEncoderState
   { getExercise :: Exercise a
   , isOpenMath  :: Bool
   , encodeTerm  :: a -> XMLBuilder
   }

attr :: String -> String -> XMLEncoder a t
attr s a = pure (s .=. a)

xmlEncoder :: XMLEncoder a (TypedValue (Type a))
xmlEncoder = choice
   [ encodeTyped (encodeDiagnosis)
   , encodeTyped (encodeDecompositionReply)
   , encodeTyped (encodeDerivation)
   , encodeTyped (encodeDerivationText)
   , encodeTyped (encodeDifficulty)
   , encodeTyped (encodeMessage)
   , encoderStateFor $ \xp tv@(val ::: tp) -> 
        case tp of
           -- meta-information
           Tag "RuleShortInfo" t ->
              case equal t (Const Rule) of
                 Just f  -> ruleShortInfo // f val
                 Nothing -> encoderError "rule short info"
           Tag "RulesInfo" _ -> 
              pure (rulesInfoXML (getExercise xp) (encodeTerm xp))
           Tag "elem" t -> 
              element "elem" <$> xmlEncoder // (val ::: t)
           -- special case for onefirst service; insert elem Tag
           Const String :|: Pair t (Const State) | isJust (equal stepInfoType t) ->
              xmlEncoder // (val ::: (Const String :|: Tag "elem" (Pair t (Const State))))
           -- special case for exceptions
           Const String :|: t -> 
              case val of
                 Left s  -> encoderError s
                 Right a -> xmlEncoder // (a ::: t)
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
           _ -> error $ show tp
   ]
 where
   stepInfoType :: Type a (StepInfo a)
   stepInfoType = typed

xmlEncoderConst :: XMLEncoder a (TypedValue (Const a))
xmlEncoderConst = encoderFor $ \tv@(val ::: tp) ->
   case tp of
      SomeExercise -> case val of
                         Some a -> exerciseInfo // a
      Strategy  -> pure (builder (strategyToXML val))
      Rule      -> attr "ruleid" (show val)
      State     -> encodeState // val
      Context   -> encodeContext // val
      Location  -> encodeLocation // val
      Environment -> encodeEnvironment // val
      Text      -> encodeText // val
      Bool      -> pure (text (showBool val))
      _         -> pure (text (show tv))

encodeState :: XMLEncoder a (State a)
encodeState = encoderFor $ \st ->
   element "state" <$>
      encodePrefixes // statePrefixes st
      <> encodeContext // stateContext st

encodePrefixes :: XMLEncoder a [Prefix (Context a)]
encodePrefixes = encoderFor $ \ps -> 
   case ps of
      [] -> mempty
      _  -> element "prefix" <$> mconcat [ pure (text (show p)) | p <- ps ]

encodeContext :: XMLEncoder a (Context a)
encodeContext = encoderStateFor $ \xp ctx -> pure $ do
   let loc    = fromLocation (location ctx)
       values = bindings (withLoc ctx)
       withLoc
          | null loc  = id
          | otherwise = insertRef (makeRef "location") loc
      
   a <- fromContext ctx
   encodeTerm xp a
   unless (null values) $ element "context" $
      forM_ values $ \tb ->
         element "item" $ do
            "name"  .=. showId tb
            case getTermValue tb of
               term | isOpenMath xp -> 
                  builder (omobj2xml (toOMOBJ term))
               _ -> "value" .=. showValue tb

encodeLocation :: XMLEncoder a Location
encodeLocation = encoderFor $ \loc -> pure ("location" .=. show loc)

encodeEnvironment :: HasEnvironment env => XMLEncoder a env
encodeEnvironment = encoderFor $ \env -> 
   mconcat [ encodeTypedBinding // b | b <- bindings env ]

encodeTypedBinding :: XMLEncoder a Binding
encodeTypedBinding = encoderStateFor $ \xp tb -> 
   element "argument" <$>
      attr "description" (showId tb)
      <> case getTermValue tb of
            term | isOpenMath xp -> pure $ builder $ 
               omobj2xml $ toOMOBJ term
            _ -> pure (text (showValue tb))

encodeDerivation :: XMLEncoder a (Derivation (Rule (Context a), Environment) (Context a))
encodeDerivation = encoderFor $ \d ->
   let xs = [ (s, a) | (_, s, a) <- triples d ]
   in xmlEncoder // (xs ::: typed)

encodeDerivationText :: XMLEncoder a (Derivation String (Context a))
encodeDerivationText = encoderFor $ \d -> encodeAsList $ 
   [ attr "ruletext" s <> encodeContext // a
   | (_, s, a) <- triples d
   ]

ruleShortInfo :: XMLEncoder a (Rule (Context a))
ruleShortInfo = simpleEncoder $ \r -> do 
   "name"        .=. showId r
   "buggy"       .=. showBool (isBuggy r)
   "arguments"   .=. show (length (getRefs r))
   "rewriterule" .=. showBool (isRewriteRule r)

encodeDifficulty :: XMLEncoder a Difficulty
encodeDifficulty = simpleEncoder $ \d -> 
   "difficulty" .=. show d

encodeText :: XMLEncoder a Text
encodeText = encoderFor $ \txt -> 
   mconcat [ encodeItem // item | item <- textItems txt ] 
 where
   encodeItem = encoderStateFor $ \xp item -> pure $ 
      case item of
         TextTerm a -> fromMaybe (text (show item)) $ do
            v <- hasTermView (getExercise xp)
            b <- match v a
            return (encodeTerm xp b)
         _ -> text (show item)

encodeMessage :: XMLEncoder a FeedbackText.Message
encodeMessage = encoderFor $ \msg ->
   element "message" <$>
      case FeedbackText.accept msg of
         Just b  -> attr "accept" (showBool b)
         Nothing -> mempty
      <> encodeText // FeedbackText.text msg

encodeDiagnosis :: XMLEncoder a (Diagnosis a)
encodeDiagnosis = encoderFor $ \diagnosis ->
   case diagnosis of
      Buggy env r -> 
         element "buggy" <$> 
            encodeEnvironment // env 
            <> attr "ruleid" (showId r)
      NotEquivalent -> 
         pure (tag "notequiv")
      Similar b st -> 
         element "similar" <$>  
            attr "ready" (showBool b) 
            <> encodeState // st
      Expected b st r -> 
         element "expected" <$>
            attr "ready" (showBool b) 
            <> encodeState // st 
            <> attr "ruleid" (showId r)
      Detour b st env r -> 
         element "detour" <$>
            attr "ready" (showBool b) 
            <> encodeState // st 
            <> encodeEnvironment // env 
            <> attr "ruleid" (showId r)
      Correct b st -> 
         element "correct" <$>
            attr "ready" (showBool b) 
            <> encodeState // st
   
encodeDecompositionReply :: XMLEncoder a (PD.Reply a)
encodeDecompositionReply = encoderFor $ \reply ->
   case reply of
      PD.Ok loc st -> 
         element "correct" <$> 
            encLoc loc
            <> encodeState // st
      PD.Incorrect eq loc st env -> 
         element "incorrect" <$>
            attr "equivalent" (showBool eq)
            <> encLoc loc
            <> encodeState // st
            <> encodeEnvironment // env
 where
    encLoc loc = element "location" <$> pure (text (show loc))

exerciseInfo :: XMLEncoder a (Exercise b)
exerciseInfo = simpleEncoder $ \ex -> do
   "exerciseid"  .=. showId ex
   "description" .=. description ex
   "status"      .=. show (status ex)

------------------------------------------------
-- helpers

encodeAsList :: [XMLEncoder a t] -> XMLEncoder a t
encodeAsList xs = element "list" <$> mconcat 
   [ element "elem" <$> x | x <- xs ]

showBool :: Bool -> String
showBool = map toLower . show