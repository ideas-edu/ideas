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
-- Abstract syntax for feedback scripts, and pretty-printer (Show instance)
--
-----------------------------------------------------------------------------

module Ideas.Service.FeedbackScript.Syntax
   ( Script, makeScript, scriptDecls, makeText, textItems
   , Decl(..), DeclType(..), Text(..), Condition(..), includes
   , feedbackDecl, textForIdDecl
   ) where

import Data.Char
import Data.List
import Data.Maybe
import Data.Semigroup as Sem
import Ideas.Common.Library
import Ideas.Utils.Uniplate

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
   | MotivationIs Id
   | CondNot   Condition
   | CondConst Bool
   | CondRef Id

makeText :: String -> Text
makeText s = case words s of
                [] -> TextEmpty
                xs -> TextString (unwords xs)

feedbackDecl, textForIdDecl :: HasId a => a -> Text -> Decl
feedbackDecl  a = Simple Feedback  [getId a]
textForIdDecl a = Simple TextForId [getId a]

includes :: Script -> [FilePath]
includes script = [ file | Include xs <- scriptDecls script, file <- xs ]

instance Show Script where
   show = unlines . map show . scriptDecls

instance Show Decl where
   show decl =
      let idList   = intercalate ", " . map show
          f dt as  = unwords [show dt, idList as]
          g (c, t) = "   | " ++ show c ++ " = " ++ nonEmpty (show t)
          nonEmpty xs = if null xs then "{}" else xs
      in case decl of
            NameSpace as     -> "namespace " ++ idList as
            Supports as      -> "supports "  ++ idList as
            Include xs       -> "include "   ++ intercalate ", " xs
            Simple dt as t   -> f dt as ++ " = " ++ nonEmpty (show t)
            Guarded dt as xs -> unlines (f dt as : map g xs)

instance Show DeclType where
   show TextForId  = "text"
   show StringDecl = "string"
   show Feedback   = "feedback"

instance Show Condition where
   show (RecognizedIs a) = "recognize " ++ show a
   show (MotivationIs a) = "motivation " ++ show a
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

instance Sem.Semigroup Script where
   s <> t = makeScript (scriptDecls s ++ scriptDecls t)

instance Monoid Script where
   mempty  = makeScript []
   mappend = (<>)

instance Sem.Semigroup Text where
   (<>) = (:<>:)

instance Monoid Text where
   mempty  = TextEmpty
   mappend = (<>)

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

-- Combine two strings by inserting a space in between (unless one of the
-- strings is empty, or when the second string starts with an interpunction
-- symbol).
combine :: String -> String -> String
combine a b
   | null a    = b
   | null b    = a
   | maybe False special (listToMaybe b) = a ++ b
   | otherwise = a ++ " " ++ b
 where
    special = (`elem` ".,:;?!")