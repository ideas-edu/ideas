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

import Common.Algebra.Group

type Script = [Decl]
type Annotation = String

data Decl = RuleText Id Text -- text for a rule (buggy, or non-buggy)

data Text = Empty
          | Text :+: Text
          | Text String  
          | CondReady Text Text     
          
instance Monoid Text where
   mempty  = Empty
   mappend = (:+:)
   
toString :: Bool -> Script -> Text -> String
toString ready script = rec
 where
   rec Empty           = []
   rec (a :+: b)       = rec a ++ rec b
   rec (Text s)        = s
   rec (CondReady a b) = rec (if ready then a else b)
   
   {-
   rec AttrRule        = maybe [] (rec . find) mrule
   
   find r = case [ t | RuleText a t <- script, a == getId r ] of
               t:_ -> t
               []  -> Text (showId r) -}
   
ifNotReady :: Text -> Text
ifNotReady a = CondReady Empty a