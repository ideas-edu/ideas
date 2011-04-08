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
import Common.Utils (commaList)
import Data.Char
import Data.Monoid

newtype Script = S { scriptDecls :: [Decl] }

makeScript :: [Decl] -> Script
makeScript = S

data Decl 
   = NameSpace [Id]
   | Supports  [Id]
   | Simple  DeclType [Id] Text
   | Guarded DeclType [Id] [(Condition, Text)]

data DeclType = RuleText | StringDecl | Feedback

type Text = [TextItem]
        
data TextItem 
   = TextString String  
   | TextRef Id
          
data Condition 
   = RecognizedIs Id
   | CondNot   Condition
   | CondConst Bool
   | CondRef Id

instance Show Script where
   show = unlines . map show . scriptDecls

instance Show Decl where 
   show decl = 
      let idList   = commaList . map show
          f dt as  = unwords [show dt, idList as]
          g (c, t) = "   | " ++ show c ++ " = " ++ texts t
          texts    = unwords . map show
      in case decl of
            NameSpace as     -> "namespace " ++ idList as
            Supports as      -> "supports "  ++ idList as
            Simple dt as t   -> f dt as ++ " = " ++ texts t
            Guarded dt as xs -> unlines (f dt as : map g xs)

instance Show DeclType where
   show RuleText   = "text"
   show StringDecl = "string"
   show Feedback   = "feedback"

instance Show Condition where
   show (RecognizedIs a) = "recognize " ++ show a
   show (CondNot c)      = "not " ++ show c
   show (CondConst b)    = map toLower (show b)
   show (CondRef a)      = "@" ++ show a 

instance Show TextItem where
   show (TextString s) = s
   show (TextRef a)    = "@" ++ show a
   
instance Monoid Script where
   mempty = makeScript []
   mappend s t = makeScript (scriptDecls s ++ scriptDecls t)