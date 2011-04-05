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
-- Abstract syntax for feedback scripts
--
-----------------------------------------------------------------------------
module Service.FeedbackScript 
   ( module Service.FeedbackScript, (<>)
   ) where

import Common.Id
import Common.Utils (safeHead)
import Control.Monad
import Common.Algebra.Group
import Common.Transformation
import Data.Maybe

type Script = [Decl]

data Environment a = Env
   { oldReady   :: Maybe Bool
   , recognized :: Maybe (Rule a)
   , expected   :: Maybe (Rule a)
   , diffPair   :: Maybe (String, String)
   }
   
emptyEnvironment :: Environment a
emptyEnvironment = Env 
   { oldReady   = Nothing
   , recognized = Nothing
   , expected   = Nothing
   , diffPair   = Nothing
   }

data Decl = RuleText   Id Text -- text for a rule (buggy, or non-buggy)
          | StringDecl (Maybe Condition) Id Text
          | Feedback   Id Text
   deriving Show
   
data Text = Empty
          | Text :+: Text
          | Text String  
          | AttrRef Id
   deriving Show
          
data Condition = RecognizedIs Id
               | CondRef Id
   deriving Show

instance Monoid Text where
   mempty  = Empty
   mappend = (:+:)

toString :: Environment a -> Script -> Text -> String
toString env script = fromMaybe "<<undefined>>" . toStringM env script

toStringM :: Environment a -> Script -> Text -> Maybe String
toStringM env script = rec
 where
   rec Empty                    = return []
   rec (a :+: b)                = liftM2 (++) (rec a) (rec b)
   rec (Text s)                 = return s
   rec (AttrRef a)              
      | a == newId "expected"   = fmap (newRuleText script) (expected env)
      | a == newId "recognized" = fmap (newRuleText script) (recognized env)
      | a == newId "diffbefore" = fmap fst (diffPair env)
      | a == newId "diffafter"  = fmap snd (diffPair env)
      | otherwise               = safeHead [ s
                                           | StringDecl c b t <- script
                                           , a == b 
                                           , maybe True evalBool c
                                           , Just s <- [rec t]
                                           ]

   evalBool (RecognizedIs a) = maybe False ((==a) . getId) (recognized env)
   evalBool (CondRef a)
      | a == newId "oldready"    = fromMaybe False (oldReady env)
      | a == newId "hasexpected" = isJust (expected env)
      | otherwise                = False
   
newRuleText :: HasId r => Script -> r -> String
newRuleText script r =
   let xs = [ s | RuleText a t <- script, a == getId r
                , Just s <- [toStringM emptyEnvironment script t] ]
   in head $ xs ++ [showId r]

feedbackFor :: Id -> Environment a -> Script -> String
feedbackFor a env script = 
   case [ t | Feedback b t  <- script, a==b ] of
      t:_ -> toString env script t
      []  -> ""

feedbackSame, feedbackNotEq, feedbackUnknown, feedbackOk,
   feedbackBuggy, feedbackDetour :: Environment a -> Script -> String
feedbackSame    = feedbackFor $ newId "same"
feedbackNotEq   = feedbackFor $ newId "noteq"
feedbackUnknown = feedbackFor $ newId "unknown"
feedbackOk      = feedbackFor $ newId "ok"
feedbackBuggy   = feedbackFor $ newId "buggy"
feedbackDetour  = feedbackFor $ newId "detour"