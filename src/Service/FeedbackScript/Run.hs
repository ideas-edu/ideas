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
import Common.Utils (safeHead)
import Control.Monad
import Common.Transformation
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
   }
   
newEnvironment :: State a -> Environment a
newEnvironment st = Env 
   { oldReady   = ready st
   , expected   = fmap fst4 next
   , recognized = Nothing
   , diffPair   = Nothing
   }
 where
   next = either (const Nothing) Just (onefirst st)
   fst4 (a, _, _, _) = a

toString :: Environment a -> Script -> Text -> String
toString env script = fromMaybe "" . eval env script . Right

ruleToString :: Environment a -> Script -> Rule b -> String
ruleToString env script = fromMaybe "" . eval env script . Left . getId

eval :: Environment a -> Script -> Either Id Text -> Maybe String
eval env script = fmap show . either (return . findIdRef) evalText
 where   
   evalText :: Text -> Maybe Text
   evalText = liftM mconcat . mapM unref . textItems
    where
      unref (TextRef a) 
         | a == newId "expected"   = fmap (findIdRef . getId) (expected env)
         | a == newId "recognized" = fmap (findIdRef . getId) (recognized env)
         | a == newId "diffbefore" = fmap (TextString . fst) (diffPair env)
         | a == newId "diffafter"  = fmap (TextString . snd) (diffPair env)
         | a `elem` feedbackIds    = findRef (==a)
         | otherwise               = findRef (==a) 
      unref t = Just t

   evalBool :: Condition -> Bool
   evalBool (RecognizedIs a) = maybe False (eqId a . getId) (recognized env)
   evalBool (CondNot c)      = not (evalBool c)
   evalBool (CondConst b)    = b
   evalBool (CondRef a)
      | a == newId "oldready"    = oldReady env
      | a == newId "hasexpected" = isJust (expected env)
      | otherwise                = False

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
      Buggy r        -> make "buggy" env {recognized = Just r}
      NotEquivalent  -> make "noteq" env
      Expected _ _ r -> make "ok" env {recognized = Just r}
      Similar _ _    -> make "same" env
      Detour _ _ r   -> make "detour" env {recognized = Just r}
      Correct _ _    -> make "unknown" env
   
feedbackHint :: Bool -> Environment a -> Script -> String
feedbackHint b = make (if b then "hint" else "step")

make :: String -> Environment a -> Script -> String
make s env script = toString env script (TextRef (newId s))

feedbackIds :: [Id]
feedbackIds = map newId 
   ["same", "noteq", "unknown", "ok", "buggy", "detour", "hint", "step"]
   
attributeIds :: [Id]
attributeIds = map newId 
   ["expected", "recognized", "diffbefore", "diffafter"]
   
conditionIds :: [Id]
conditionIds = map newId
    ["oldready", "hasexpected"]