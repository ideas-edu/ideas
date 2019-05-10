-----------------------------------------------------------------------------
-- Copyright 2018, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Datatype for representing XML documents
--
-----------------------------------------------------------------------------

module Ideas.Text.XML.Document
   ( Name, Attributes, Attribute(..), Reference(..), Parameter(..)
   , XMLDoc(..), XML(..), Element(..), Content, DTD(..), DocTypeDecl(..)
   , ContentSpec(..), CP(..), AttType(..), DefaultDecl(..), AttDef
   , EntityDef, AttValue, EntityValue, ExternalID(..), PublicID
   , Conditional(..), TextDecl, External
   , prettyXML, prettyElement, escape
   ) where

import Prelude hiding ((<$>))

import Text.PrettyPrint.Leijen

type Name = String

type Attributes = [Attribute]
data Attribute  = Name := AttValue

data Reference = CharRef Int | EntityRef String

newtype Parameter = Parameter String

data XMLDoc = XMLDoc
   { versionInfo :: Maybe String
   , encoding    :: Maybe String
   , standalone  :: Maybe Bool
   , dtd         :: Maybe DTD
   , externals   :: [(String, External)]
   , root        :: Element
   }

data XML = Tagged Element
         | CharData String
         | CDATA String
         | Reference Reference

data Element = Element
   { name       :: Name
   , attributes :: Attributes
   , content    :: Content
   }

type Content = [XML]

data DTD = DTD Name (Maybe ExternalID) [DocTypeDecl]

data DocTypeDecl = ElementDecl Name ContentSpec
                 | AttListDecl Name [AttDef]
                 | EntityDecl Bool Name EntityDef
                 | NotationDecl Name (Either ExternalID PublicID)
                 | DTDParameter Parameter
                 | DTDConditional Conditional

data ContentSpec = Empty | Any | Mixed Bool [Name] | Children CP

-- content particles
data CP = Choice [CP] | Sequence [CP] | QuestionMark CP | Star CP | Plus CP | CPName Name

data AttType = IdType | IdRefType | IdRefsType | EntityType | EntitiesType | NmTokenType | NmTokensType
             | StringType | EnumerationType [String] | NotationType [String]

data DefaultDecl = Required | Implied | Value AttValue | Fixed AttValue

type AttDef = (Name, AttType, DefaultDecl)
type EntityDef = Either EntityValue (ExternalID, Maybe String)
type AttValue    = [Either Char Reference]
type EntityValue = [Either Char (Either Parameter Reference)]

data ExternalID = System String | Public String String

type PublicID = String

data Conditional = Include [DocTypeDecl] | Ignore [String]

type TextDecl = (Maybe String, String)

type External = (Maybe TextDecl, Content)

------------------------------------------------------------------
-- Showing

instance Show Attribute where show = show . pretty
instance Show Reference where show = show . pretty
instance Show Parameter where show = show . pretty
instance Show XML       where show = show . pretty
instance Show Element   where show = show . pretty

------------------------------------------------------------------
-- Pretty printing

instance Pretty Attribute  where
   pretty (n := v) = text n <> char '=' <> prettyAttValue v

instance Pretty Reference where
   pretty ref =
      case ref of
         CharRef n   -> text "&#" <> int n <> char ';'
         EntityRef s -> char '&' <> text s <> char ';'

instance Pretty Parameter where
   pretty (Parameter s) = text "%" <> text s <> text ";"

instance Pretty XML where
   pretty = prettyXML False

instance Pretty Element where
   pretty = prettyElement False

prettyXML :: Bool -> XML -> Doc
prettyXML compact xml =
   case xml of
      Tagged e    -> prettyElement compact e
      CharData s  -> text (escape s)
      CDATA s     -> text "<![CDATA[" <> text s <> text "]]>"
      Reference r -> pretty r

prettyElement :: Bool -> Element -> Doc
prettyElement compact (Element n as c)
   | null c    = openCloseTag n as
   | compact   = make (<>)
   | otherwise = make (<$>)
 where
   make op = let body  = foldr1 op (map (prettyXML compact) c)
                 ibody = (if compact then id else indent 2) body
             in openTag n as `op` ibody `op` closeTag n

openTag :: Name -> Attributes -> Doc
openTag = prettyTag (char '<') (char '>')

openCloseTag :: Name -> Attributes -> Doc
openCloseTag = prettyTag (char '<') (text "/>")

closeTag :: Name -> Doc
closeTag n = prettyTag (text "</") (char '>') n []

prettyTag :: Doc -> Doc -> Name -> Attributes -> Doc
prettyTag open close n as = open <> hsep (text n:map pretty as) <> close

prettyAttValue :: AttValue -> Doc
prettyAttValue = dquotes . hcat . map (either (text . escapeChar) pretty)

escape :: String -> String
escape = concatMap escapeChar

escapeChar :: Char -> String
escapeChar c =
   case c of
      '<'  -> "&lt;"
      '>'  -> "&gt;"
      '&'  -> "&amp;"
      '"'  -> "&quot;"
      '\'' -> "&apos;"
      _    -> [c]