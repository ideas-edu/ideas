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
-- Abstract syntax for feedback scripts, and pretty-printer (Show instance)
--
-----------------------------------------------------------------------------
module Service.FeedbackScript.Syntax 
   ( Script, makeScript, scriptDecls
   , Decl(..), DeclType(..), Text, TextItem(..), Condition(..)
   ) where

import Common.Id
import Data.Monoid

newtype Script = S { scriptDecls :: [Decl] }

makeScript :: [Decl] -> Script
makeScript = S

data Decl 
   = NameSpace Id
   | Decl DeclType Id (Maybe Condition) Text

data DeclType = RuleText | StringDecl | Feedback

type Text = [TextItem]
        
data TextItem 
   = TextString String  
   | TextRef Id
          
data Condition 
   = RecognizedIs Id
   | CondRef Id

instance Show Script where
   show = unlines . map show . scriptDecls

instance Show Decl where 
   show (NameSpace a)   = "namespace " ++ show a
   show (Decl dt a c t) = unwords $
      [ show dt, show a] ++ 
      maybe [] (\x -> ["|" , show x]) c ++ 
      ("=" : map show t)

instance Show DeclType where
   show RuleText   = "text"
   show StringDecl = "string"
   show Feedback   = "feedback"

instance Show Condition where
   show (RecognizedIs a) = "recognize " ++ show a
   show (CondRef a)      = "@" ++ show a 

instance Show TextItem where
   show (TextString s) = s
   show (TextRef a)    = "@" ++ show a
   
instance Monoid Script where
   mempty = makeScript []
   mappend s t = makeScript (scriptDecls s ++ scriptDecls t)