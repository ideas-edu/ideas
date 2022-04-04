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
-- Datatype for representing XML documents
--
-----------------------------------------------------------------------------

module Ideas.Text.XML.Document
   ( Name, toName, uncheckedName
   , Attributes, Attribute(..), Reference(..), Parameter(..)
   , XMLDoc(..), XML(..), Element(..), Content, DTD(..), DocTypeDecl(..)
   , ContentSpec(..), CP(..), AttType(..), DefaultDecl(..), AttDef
   , EntityDef, AttValue, EntityValue, ExternalID(..), PublicID
   , Conditional(..), TextDecl, External
   , prettyXML, prettyElement, escape
   ) where

import Data.String
import Prelude hiding ((<$>))
import Ideas.Text.XML.Unicode
import Text.PrettyPrint.Leijen

------------------------------------------------------------------
-- (checked) names

newtype Name = N String
 deriving (Eq, Ord)

instance Show Name where
   show (N s) = s

instance IsString Name where
   fromString = toName

toName :: String -> Name
toName s 
   | validName s = N s
   | otherwise   = error $ "Invalid XML name: " ++ s

uncheckedName :: String -> Name
uncheckedName = N

validName :: String -> Bool
validName []     = False
validName (x:xs) = (isLetter x || x `elem` "_:") && all validNameChar xs

validNameChar :: Char -> Bool
validNameChar c = any ($ c) [isLetter, isDigit, isCombiningChar, isExtender, (`elem` ".-_:")]

------------------------------------------------------------------
-- (checked) names

type Attributes = [Attribute]
data Attribute  = Name := AttValue

data Reference = CharRef Int | EntityRef Name

newtype Parameter = Parameter Name

data XMLDoc = XMLDoc
   { versionInfo :: Maybe String
   , encoding    :: Maybe String
   , standalone  :: Maybe Bool
   , dtd         :: Maybe DTD
   , externals   :: [(Name, External)]
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
             | StringType | EnumerationType [String] | NotationType [Name]

data DefaultDecl = Required | Implied | Value AttValue | Fixed AttValue

type AttDef = (Name, AttType, DefaultDecl)
type EntityDef = Either EntityValue (ExternalID, Maybe Name)
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

instance Pretty Name where
   pretty = text . show

instance Pretty Attribute where
   pretty (n := v) = pretty n <> char '=' <> prettyAttValue v

instance Pretty Reference where
   pretty ref =
      case ref of
         CharRef n   -> text "&#" <> int n <> char ';'
         EntityRef s -> char '&' <> pretty s <> char ';'

instance Pretty Parameter where
   pretty (Parameter s) = text "%" <> pretty s <> text ";"

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
prettyElement _ (Element n as [CharData s]) | show n == "script" =
   -- quick fix for not escaping javascript code in html
   openTag n as <> text s <> closeTag n
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
prettyTag open close n as = open <> hsep (pretty n:map pretty as) <> close

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