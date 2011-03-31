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
import Control.Monad
import Common.Algebra.Group
import Common.Transformation
import Data.Maybe

type Script = [Decl]
type Annotation = String

data Environment a = Env
   { oldReady   :: Maybe Bool
   , recognized :: Maybe (Rule a)
   , expected   :: Maybe (Rule a)
   }
   
emptyEnvironment :: Environment a
emptyEnvironment = Env 
   { oldReady   = Nothing
   , recognized = Nothing
   , expected   = Nothing
   }

data Decl = RuleText Id Text -- text for a rule (buggy, or non-buggy)
          | FeedbackSame Text
          | FeedbackNotEq Text
          | FeedbackUnknown Text
          | FeedbackOk Text
          | FeedbackBuggy Text
          | FeedbackDetour Text

data Text = Empty
          | Text :+: Text
          | Text String  
          | CondOldReady Text Text
          | CondHasExpected Text Text
          | CondRecognizedIs Id Text Text 
          | AttrRecognized
          | AttrExpected
          
instance Monoid Text where
   mempty  = Empty
   mappend = (:+:)

ifNotOldReady :: Text -> Text
ifNotOldReady a = CondOldReady Empty a

toString :: Environment a -> Script -> Text -> String
toString env script = fromMaybe "<<undefined>>" . toStringM env script

toStringM :: Environment a -> Script -> Text -> Maybe String
toStringM env script = rec
 where
   rec Empty                    = return []
   rec (a :+: b)                = liftM2 (++) (rec a) (rec b)
   rec (Text s)                 = return s
   rec (CondOldReady a b)       = oldReady env >>= \x -> 
                                  if x then rec a else rec b
   rec (CondHasExpected a b)    = rec (if isJust (expected env) then a else b)
   rec (CondRecognizedIs x a b) = rec (if maybe False ((==x) . getId) (recognized env) then a else b) 
   rec AttrRecognized           = fmap (newRuleText script) (recognized env)
   rec AttrExpected             = fmap (newRuleText script) (expected env)
   
newRuleText :: HasId r => Script -> r -> String
newRuleText script r =
   let xs = [ s | RuleText a t <- script, a == getId r
                , Just s <- [toStringM emptyEnvironment script t] ]
   in head $ xs ++ [showId r]

feedbackSame :: Environment a -> Script -> String
feedbackSame env script = 
   case [ t | FeedbackSame t  <- script ] of
      t:_ -> toString env script t
      []  -> ""

feedbackNotEq :: Environment a -> Script -> String
feedbackNotEq env script = 
   case [ t | FeedbackNotEq t  <- script ] of
      t:_ -> toString env script t
      []  -> ""

feedbackUnknown :: Environment a -> Script -> String
feedbackUnknown env script = 
   case [ t | FeedbackUnknown t  <- script ] of
      t:_ -> toString env script t
      []  -> ""
      
feedbackOk :: Environment a -> Script -> String
feedbackOk env script = 
   case [ t | FeedbackOk t  <- script ] of
      t:_ -> toString env script t
      []  -> ""
      
feedbackBuggy :: Environment a -> Script -> String
feedbackBuggy env script = 
   case [ t | FeedbackBuggy t  <- script ] of
      t:_ -> toString env script t
      []  -> ""
      
feedbackDetour :: Environment a -> Script -> String
feedbackDetour env script = 
   case [ t | FeedbackDetour t  <- script ] of
      t:_ -> toString env script t
      []  -> ""