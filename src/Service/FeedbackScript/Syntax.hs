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
   ( Script, makeScript, scriptDecls, makeText, textItems
   , Decl(..), DeclType(..), Text(..), Condition(..)
   , feedbackDecl, textForIdDecl
   , module Data.Monoid, (<>)
   ) where

import Common.Algebra.Group ((<>))
import Common.Library
import Common.Utils.Uniplate
import Common.Utils (commaList, safeHead)
import Data.Char
import Data.Monoid

newtype Script = S { scriptDecls :: [Decl] }

makeScript :: [Decl] -> Script
makeScript = S

data Decl 
   = NameSpace [Id]
   | Supports  [Id]
   | Include [FilePath]
   | Simple  DeclType [Id] Text
   | Guarded DeclType [Id] [(Condition, Text)]

data DeclType = TextForId | StringDecl | Feedback
        
data Text
   = TextString String  
   | TextTerm   Term
   | TextRef Id
   | TextEmpty
   | Text :<>: Text
          
data Condition 
   = RecognizedIs Id
   | CondNot   Condition
   | CondConst Bool
   | CondRef Id

makeText :: String -> Text
makeText s = case words s of
                [] -> TextEmpty
                xs -> TextString (combineList xs)

feedbackDecl, textForIdDecl :: HasId a => a -> Text -> Decl
feedbackDecl  a = Simple Feedback  [getId a]
textForIdDecl a = Simple TextForId [getId a]

instance Show Script where
   show = unlines . map show . scriptDecls

instance Show Decl where 
   show decl = 
      let idList   = commaList . map show
          f dt as  = unwords [show dt, idList as]
          g (c, t) = "   | " ++ show c ++ " = " ++ nonEmpty (show t)
          nonEmpty xs = if null xs then "{}" else xs
      in case decl of
            NameSpace as     -> "namespace " ++ idList as
            Supports as      -> "supports "  ++ idList as
            Include xs       -> "include "   ++ commaList xs
            Simple dt as t   -> f dt as ++ " = " ++ nonEmpty (show t)
            Guarded dt as xs -> unlines (f dt as : map g xs)

instance Show DeclType where
   show TextForId  = "text"
   show StringDecl = "string"
   show Feedback   = "feedback"

instance Show Condition where
   show (RecognizedIs a) = "recognize " ++ show a
   show (CondNot c)      = "not " ++ show c
   show (CondConst b)    = map toLower (show b)
   show (CondRef a)      = '@' : show a 

instance Show Text where
   show (TextString s) = s
   show (TextTerm a)   = show a
   show TextEmpty      = ""
   show t@(_ :<>: _)   = show [t]
   show (TextRef a)    = '@' : show a
   
   showList xs ys = 
      foldr (combine . show) ys (concatMap textItems xs)
   
instance Monoid Script where
   mempty = makeScript []
   mappend s t = makeScript (scriptDecls s ++ scriptDecls t)
   
instance Monoid Text where
   mempty  = TextEmpty
   mappend = (:<>:)
   
instance Uniplate Condition where
   uniplate (CondNot a) = plate CondNot |* a
   uniplate c           = plate c

instance Uniplate Text where
   uniplate (a :<>: b) = plate (:<>:) |* a |* b
   uniplate t          = plate t

textItems :: Text -> [Text]
textItems t = rec t []
 where
   rec (a :<>: b) = rec a . rec b
   rec TextEmpty  = id
   rec a          = (a:)

combineList :: [String] -> String
combineList = foldr combine []

combine :: String -> String -> String
combine a b 
   | null a    = b
   | null b    = a
   | maybe False special (safeHead b) = a ++ b
   | otherwise = a ++ " " ++ b
 where
    special = (`elem` ".,:;?!")