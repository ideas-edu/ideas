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
-- Run a feedbackscript
--
-----------------------------------------------------------------------------

module Ideas.Service.FeedbackScript.Run
   ( Script
   , Environment(..), newEnvironment
   , feedbackDiagnosis, feedbackHint, feedbackHints
   , ruleToString, feedbackIds, attributeIds, conditionIds
   , eval
   ) where

import Data.List
import Data.Maybe
import Ideas.Common.Library hiding (ready, Environment)
import Ideas.Service.BasicServices
import Ideas.Service.Diagnose
import Ideas.Service.FeedbackScript.Syntax
import Ideas.Service.State

data Environment a = Env
   { oldReady   :: Bool
   , expected   :: Maybe (Rule (Context a))
   , recognized :: Maybe (Rule (Context a))
   , motivation :: Maybe (Rule (Context a))
   , diffPair   :: Maybe (String, String)
   , before     :: Maybe Term
   , after      :: Maybe Term
   , afterText  :: Maybe String
   }

newEnvironment :: State a -> Maybe (Rule (Context a)) -> Environment a
newEnvironment st motivationRule = newEnvironmentFor st motivationRule next
  where
    next = either (const Nothing) Just (onefirst st)

newEnvironmentFor :: State a -> Maybe (Rule (Context a)) -> Maybe ((Rule (Context a), b, c), State a) -> Environment a
newEnvironmentFor st motivationRule next = Env
  { oldReady   = finished st
  , expected   = fmap (\((x,_,_),_) -> x) next
  , motivation = motivationRule
  , recognized = Nothing
  , diffPair   = Nothing
  , before     = f st
  , after      = fmap snd next >>= f
  , afterText  = fmap snd next >>= g
  }
 where
  f s  = fmap (`build` stateTerm s) (hasTermView (exercise s))
  g s  = return $ prettyPrinter (exercise s) (stateTerm s)

toText :: Environment a -> Script -> Text -> Maybe Text
toText env script = eval env script . Right

ruleToString :: Environment a -> Script -> Rule b -> String
ruleToString env script r =
   let f = eval env script . Left . getId
   in maybe (showId r) show (f r)

eval :: Environment a -> Script -> Either Id Text -> Maybe Text
eval env script = either (return . findIdRef) evalText
 where
   evalText :: Text -> Maybe Text
   evalText = fmap mconcat . mapM unref . textItems
    where
      unref (TextRef a)
         | a == expectedId   = fmap (findIdRef . getId) (expected env)
         | a == recognizedId = fmap (findIdRef . getId) (recognized env)
         | a == diffbeforeId = fmap (TextString . fst) (diffPair env)
         | a == diffafterId  = fmap (TextString . snd) (diffPair env)
         | a == beforeId     = fmap TextTerm (before env)
         | a == afterId      = fmap TextTerm (after env)
         | a == afterTextId  = fmap TextString (afterText env)
         | a == motivationId = fmap (findIdRef . getId) (motivation env)
         | otherwise         = findRef (==a)
      unref t = Just t

   evalBool :: Condition -> Bool
   evalBool (RecognizedIs a) = maybe False (eqId a . getId) (recognized env)
   evalBool (MotivationIs a) = maybe False (eqId a . getId) (motivation env)
   evalBool (CondNot c)      = not (evalBool c)
   evalBool (CondConst b)    = b
   evalBool (CondRef a)
      | a == oldreadyId        = oldReady env
      | a == hasexpectedId     = isJust (expected env)
      | a == hasrecognizedId   = isJust (recognized env)
      | a == hasmotivationId   = isJust (motivation env)
      | a == recognizedbuggyId = maybe False isBuggy (recognized env)
      | otherwise              = False

   namespaces = nub $ mempty : [ a | NameSpace as <- scriptDecls script, a <- as ]

   -- equality with namespaces
   eqId :: Id -> Id -> Bool
   eqId a b = any (\n -> n#a == b) namespaces

   findIdRef :: Id -> Text
   findIdRef x = fromMaybe (TextString (showId x)) (findRef (`eqId` x))

   findRef :: (Id -> Bool) -> Maybe Text
   findRef p = listToMaybe $ catMaybes
      [ evalText t
      | (as, c, t) <- allDecls
      , any p as && evalBool c
      ]

   allDecls =
      let f (Simple _ as t)   = [ (as, CondConst True, t) ]
          f (Guarded _ as xs) = [ (as, c, t) | (c, t) <- xs ]
          f _ = []
      in concatMap f (scriptDecls script)

feedbackDiagnosis :: Diagnosis a -> Environment a -> Script -> Text
feedbackDiagnosis diagnosis env =
   case diagnosis of
      SyntaxError s    -> const (makeText s)
      Buggy _ r        -> makeWrong "buggy"     env {recognized = Just r}
      NotEquivalent s  -> makeNotEq s "noteq"   env
      Expected _ _ r   -> makeOk    "ok"        env {recognized = Just r}
      WrongRule _ _ mr -> makeWrong "wrongrule" env {recognized = mr}
      Similar _ _ mr   -> makeOk    "same"      env {recognized = mr}
      Detour _ _ _ r   -> makeOk    "detour"    env {recognized = Just r}
      Correct _ _      -> makeOk    "correct"   env
      Unknown _ _      -> makeOk    "unknown"   env
 where
   makeOk    = makeDefault "Well done!"
   makeWrong = makeDefault "This is incorrect."
   makeNotEq s = if null s then makeWrong else makeDefault s
   makeDefault dt s e = fromMaybe (TextString dt) . make (newId s) e

feedbackHint :: Id -> Environment a -> Script -> Text
feedbackHint feedbackId env script =
   fromMaybe (defaultHint env script) $ make feedbackId env script

feedbackHints :: Id -> [((Rule (Context a), b, c), State a)] -> State a -> Maybe (Rule (Context a)) -> Script -> [Text]
feedbackHints feedbackId nexts state motivationRule script =
   map (\env -> fromMaybe (defaultHint env script) $
     make feedbackId env script) envs
  where
    envs = map (newEnvironmentFor state motivationRule . Just) nexts

defaultHint :: Environment a -> Script -> Text
defaultHint env script = makeText $
   case expected env of
      Just r  -> ruleToString env script r
      Nothing -> "Sorry, no hint available."

make :: Id -> Environment a -> Script -> Maybe Text
make feedbackId env script = toText env script (TextRef feedbackId)

feedbackIds :: [Id]
feedbackIds = map newId
   ["same", "noteq", "correct", "unknown", "ok", "buggy", "detour", "wrongrule", "hint", "step", "label"]

attributeIds :: [Id]
attributeIds =
   [expectedId, recognizedId, diffbeforeId, diffafterId, beforeId, afterId, afterTextId, motivationId]

conditionIds :: [Id]
conditionIds = [oldreadyId, hasexpectedId, hasrecognizedId, hasmotivationId, recognizedbuggyId]

expectedId, recognizedId, diffbeforeId, diffafterId, beforeId, afterId, afterTextId, motivationId :: Id
expectedId   = newId "expected"
recognizedId = newId "recognized"
diffbeforeId = newId "diffbefore"
diffafterId  = newId "diffafter"
beforeId     = newId "before"
afterId      = newId "after"
afterTextId  = newId "aftertext"
motivationId = newId "motivation"

oldreadyId, hasexpectedId, hasrecognizedId, hasmotivationId, recognizedbuggyId :: Id
oldreadyId        = newId "oldready"
hasexpectedId     = newId "hasexpected"
hasrecognizedId   = newId "hasrecognized"
hasmotivationId   = newId "hasmotivation"
recognizedbuggyId = newId "recognizedbuggy"