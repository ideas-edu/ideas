-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Run a feedbackscript
--
-----------------------------------------------------------------------------
module Service.FeedbackScript.Run 
   ( Script
   , Environment(..), newEnvironment
   , feedbackDiagnosis, feedbackHint
   , ruleToString, feedbackIds, attributeIds, conditionIds
   ) where

import Common.Context (Context)
import Common.Id
import Common.Exercise
import Common.Rewriting.Term
import Common.Utils (safeHead)
import Control.Monad
import Common.Transformation
import Common.View
import Data.List
import Data.Maybe
import Data.Monoid
import Service.BasicServices
import Service.FeedbackScript.Syntax
import Service.Diagnose
import Service.State

data Environment a = Env
   { oldReady   :: Bool
   , expected   :: Maybe (Rule (Context a))
   , recognized :: Maybe (Rule (Context a))
   , diffPair   :: Maybe (String, String)
   , before     :: Maybe Term
   , after      :: Maybe Term
   }
   
newEnvironment :: State a -> Environment a
newEnvironment st = Env 
   { oldReady   = ready st
   , expected   = fmap fst4 next
   , recognized = Nothing
   , diffPair   = Nothing
   , before     = f st
   , after      = liftM fth4 next >>= f
   }
 where
   next = either (const Nothing) Just (onefirst st)
   f s  = fmap (`build` stateTerm s) (hasTermView (exercise s))
   fst4 (a, _, _, _) = a
   fth4 (_, _, _, a) = a

toString :: Environment a -> Script -> Text -> String -- temporarily
toString env script = show . toText env script

toText :: Environment a -> Script -> Text -> Text
toText env script = fromMaybe mempty . eval env script . Right -- mempty for error situation

ruleToString :: Environment a -> Script -> Rule b -> String
ruleToString env script = maybe "" show . eval env script . Left . getId

eval :: Environment a -> Script -> Either Id Text -> Maybe Text
eval env script = either (return . findIdRef) evalText
 where   
   evalText :: Text -> Maybe Text
   evalText = liftM mconcat . mapM unref . textItems
    where
      unref (TextRef a) 
         | a == expectedId   = fmap (findIdRef . getId) (expected env)
         | a == recognizedId = fmap (findIdRef . getId) (recognized env)
         | a == diffbeforeId = fmap (TextString . fst) (diffPair env)
         | a == diffafterId  = fmap (TextString . snd) (diffPair env)
         | a == beforeId     = fmap TextTerm (before env)
         | a == afterId      = fmap TextTerm (after env)
         | otherwise         = findRef (==a) 
      unref t = Just t

   evalBool :: Condition -> Bool
   evalBool (RecognizedIs a) = maybe False (eqId a . getId) (recognized env)
   evalBool (CondNot c)      = not (evalBool c)
   evalBool (CondConst b)    = b
   evalBool (CondRef a)
      | a == oldreadyId    = oldReady env
      | a == hasexpectedId = isJust (expected env)
      | otherwise          = False

   namespaces = nub $ mempty : [ a | NameSpace as <- scriptDecls script, a <- as ]

   -- equality with namespaces
   eqId :: Id -> Id -> Bool
   eqId a b = any (\n -> n#a == b) namespaces

   findIdRef :: Id -> Text
   findIdRef x = fromMaybe (TextString (showId x)) (findRef (`eqId` x))
        
   findRef :: (Id -> Bool) -> Maybe Text
   findRef p = safeHead $ catMaybes
      [ evalText t
      | (as, c, t) <- allDecls
      , any p as && evalBool c
      ]
      
   allDecls = 
      let f (Simple _ as t)   = [ (as, CondConst True, t) ]
          f (Guarded _ as xs) = [ (as, c, t) | (c, t) <- xs ] 
          f _ = []
      in concatMap f (scriptDecls script)

feedbackDiagnosis :: Diagnosis a -> Environment a -> Script -> String
feedbackDiagnosis diagnosis env = 
   case diagnosis of
      Buggy r        -> make "buggy"   env {recognized = Just r}
      NotEquivalent  -> make "noteq"   env
      Expected _ _ r -> make "ok"      env {recognized = Just r}
      Similar _ _    -> make "same"    env
      Detour _ _ r   -> make "detour"  env {recognized = Just r}
      Correct _ _    -> make "unknown" env
   
feedbackHint :: Bool -> Environment a -> Script -> Text
feedbackHint b = make2 (if b then "hint" else "step")

make :: String -> Environment a -> Script -> String
make s env script = toString env script (TextRef (newId s))

make2 :: String -> Environment a -> Script -> Text
make2 s env script = toText env script (TextRef (newId s))

feedbackIds :: [Id]
feedbackIds = map newId 
   ["same", "noteq", "unknown", "ok", "buggy", "detour", "hint", "step"]
   
attributeIds :: [Id]
attributeIds =
   [expectedId, recognizedId, diffbeforeId, diffafterId, beforeId, afterId]
   
conditionIds :: [Id]
conditionIds = [oldreadyId, hasexpectedId]
    
expectedId, recognizedId, diffbeforeId, diffafterId, beforeId, afterId :: Id
expectedId   = newId "expected"  
recognizedId = newId "recognized"
diffbeforeId = newId "diffbefore"
diffafterId  = newId "diffafter" 
beforeId     = newId "before"    
afterId      = newId "after"     

oldreadyId, hasexpectedId :: Id
oldreadyId    = newId "oldready" 
hasexpectedId = newId "hasexpected"