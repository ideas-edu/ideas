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
module Ideas.Service.EncoderXML (xmlEncoder, encodeState) where

import Control.Monad
import Data.Char
import Data.Maybe
import Ideas.Common.Library hiding (exerciseId, (:=))
import Ideas.Common.Utils (Some(..))
import Ideas.Service.Evaluator
import Ideas.Service.FeedbackScript.Syntax
import Ideas.Service.OpenMathSupport
import Ideas.Service.RulesInfo (rulesInfoXML)
import Ideas.Service.State
import Ideas.Service.StrategyInfo
import Ideas.Service.Types
import Ideas.Text.OpenMath.Object
import Ideas.Text.XML
import qualified Data.Map as M
import qualified Ideas.Service.FeedbackText as FeedbackText

xmlEncoder :: Bool -> (a -> XMLBuilder) -> Exercise a -> Encoder (Type a) XMLBuilderM ()
xmlEncoder isOM enc ex tv@(val ::: tp) =
   case tp of
      -- meta-information
      Tag "RuleShortInfo" t -> do
         f <- equalM t (Const Rule)
         ruleShortInfo (f val)
      Tag "difficulty" (Iso iso (Const String)) ->
         "difficulty" .=. to iso val
      -- special case for onefirst service; insert elem Tag
      Const String :|: Pair t (Const State) | isJust (equal stepInfoType t) ->
         rec (val ::: (Const String :|: elemType (Pair t (Const State))))
      -- special case for exceptions
      Const String :|: t -> 
         case val of
            Left s  -> fail s
            Right a -> rec (a ::: t)
      -- special cases for lists
      List t -> encodeAsList (map (\a -> rec (a ::: t)) val)
      --
      Pair t1 t2 -> do rec (fst val ::: t1)
                       rec (snd val ::: t2)
      _ -> encodeWith (xmlEncoderMap isOM ex enc) (xmlEncoderConst isOM enc ex) tv
 where
   rec = xmlEncoder isOM enc ex

encodeAsList :: [XMLBuilder] -> XMLBuilder
encodeAsList = element "list" . mapM_ (element "elem")

xmlEncoderMap :: Bool -> Exercise a -> (a -> XMLBuilder) -> EncoderMap (Const a) XMLBuilderM ()
xmlEncoderMap isOM ex enc = M.fromList $
   [ ("RulesInfo", \_ -> rulesInfoXML ex enc)
   , ("message", \(val ::: tp) -> do
         f <- equalM (Tag "message" tp) (typed :: Type a FeedbackText.Message)
         let msg = f val
         element "message" $ do
            case FeedbackText.accept msg of
               Just b  -> "accept" .=. showBool b
               Nothing -> return ()
            encodeText enc ex (FeedbackText.text msg))
   , ("LocationId", \(val ::: tp) -> do
        f <- equalM tp (Const Id)
        element "location"$ text $ show $ f val)
   , ("buggy", \tv@(val ::: tp) -> 
        case useAttribute tp of
           Just f -> "buggy" .=. f val
           _ -> element "buggy" (xmlEncoder isOM enc ex tv))
   ] ++
   -- extra elements
   [ (s, element s . xmlEncoder isOM enc ex)
   | s <- [ "list", "elem", "state", "prefix"
          , "similar", "notequiv", "expected", "detour"
          , "correct", "incorrect"]
   ] ++
   -- extra attributes
   [ (s, \(val ::: tp) -> do 
        f <- useAttribute tp
        s .=. f val)
   | s <- [ "name", "arguments", "rewriterule", "accept"
          , "exerciseid", "description", "status", "ready", "ruleid"
          , "ruletext", "equivalent"]
   ]

xmlEncoderConst :: Bool -> (a -> XMLBuilder) -> Exercise a -> Encoder (Const a) XMLBuilderM ()
xmlEncoderConst isOM enc ex (val ::: tp) =
   case tp of
      Exercise  -> return ()
      SomeExercise -> case val of
                         Some a -> exerciseInfo a
      Strategy  -> builder (strategyToXML val)
      Rule      -> "ruleid" .=. show val -- encodeRule val)
      State     -> encodeState isOM enc val
      Context   -> encodeContext isOM enc val
      -- Special case for derivationtext
      Derivation (Const String) t -> 
         xmlEncoderConst isOM enc ex (val ::: Derivation (Tag "ruletext" (Const String)) t)     
      Derivation t1 t2 ->
         let xs = map (\(_, s, a) -> (s, a)) (triples val)
         in xmlEncoder isOM enc ex (xs ::: List (Pair t1 t2))
      Location  -> "location" .=. show val
      Environment -> mapM_ (encodeTypedBinding isOM) (bindings val)
      Text      -> encodeText enc ex val
      Bool      -> text (showBool val)
      Int       -> text (show val)
      String    -> text val
      _         -> fail $ "Type " ++ show tp ++ " not supported in XML"

useAttribute :: Monad m => Type a t -> m (t -> String)
useAttribute (Const String) = return id
useAttribute (Const Bool)   = return showBool
useAttribute (Const Int)    = return show
useAttribute _              = fail "not a primitive type"

encodeState :: Bool -> (a -> XMLBuilder) -> State a -> XMLBuilder
encodeState isOM enc st = element "state" $ do
   mapM_ (element "prefix" . text . show) (statePrefixes st)
   encodeContext isOM enc (stateContext st)

ruleShortInfo :: Rule a -> XMLBuilder
ruleShortInfo r = do 
   "name"        .=. showId r
   "buggy"       .=. showBool (isBuggy r)
   "arguments"   .=. show (length (getRefs r))
   "rewriterule" .=. showBool (isRewriteRule r)

encodeEnvironment :: Bool -> Context a -> XMLBuilder
encodeEnvironment isOM ctx
   | null values = return ()
   | otherwise = element "context" $
        forM_ values $ \tb ->
           element "item" $ do
              "name"  .=. showId tb
              case getTermValue tb of
                 term | isOM -> 
                    builder (omobj2xml (toOMOBJ term))
                 _ -> "value" .=. showValue tb
 where
   loc    = fromLocation (location ctx)
   values = bindings (withLoc ctx)
   withLoc
      | null loc  = id
      | otherwise = insertRef (makeRef "location") loc 

encodeContext :: Bool -> (a -> XMLBuilder) -> Context a -> XMLBuilder
encodeContext isOM f ctx = do
   a <- fromContext ctx
   f a
   encodeEnvironment isOM ctx

encodeTypedBinding :: Bool -> Binding -> XMLBuilder
encodeTypedBinding b tb = element "argument" $ do
   "description" .=. showId tb
   case getTermValue tb of
      term | b -> builder $ 
         omobj2xml $ toOMOBJ term
      _ -> text (showValue tb)
   
encodeText :: (a -> XMLBuilder) -> Exercise a -> Text -> XMLBuilder
encodeText f ex = mapM_ make . textItems
 where
   make t@(TextTerm a) = fromMaybe (text (show t)) $ do
      v <- hasTermView ex
      b <- match v a
      return (f b)
   make a = text (show a)

exerciseInfo :: Exercise a -> XMLBuilder 
exerciseInfo ex = do
   "exerciseid"  .=. showId ex
   "description" .=. description ex
   "status"      .=. show (status ex)

showBool :: Bool -> String
showBool = map toLower . show