-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
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
   ) where

type Name = String

type Attributes = [Attribute]
data Attribute  = Name := AttValue

data Reference = CharRef Int | EntityRef String

data Parameter = Parameter String

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

---
{-
instance Show XMLDoc where
   show doc = showXMLDecl doc ++ maybe "" show (dtd doc) ++ show (root doc)
-}
instance Show Attribute where
   show (n := v) = n ++ "=" ++ showAttValue v

instance Show Element where
   show (Element n as c)
      | null c    = showOpenTag True n as
      | otherwise = showOpenTag False n as ++ concatMap show c ++ showCloseTag n

instance Show XML where
   show xml =
      case xml of
         Tagged e    -> show e
         CharData s  -> s
         CDATA s     -> "<![CDATA[" ++ s ++ "]]>"
         Reference r -> show r

instance Show Reference where
   show ref =
      case ref of
         CharRef n   -> "&#" ++ show n ++ ";"
         EntityRef s -> "&" ++ s ++ ";"

instance Show Parameter where
   show (Parameter s) = "%" ++ s ++ ";"
{-
instance Show DTD where
   show (DTD n mid ds) = "<!DOCTYPE " ++ unwords list ++ ">"
    where
      list = n : catMaybes [fmap show mid, showDecls ds]
      showDecls xs
         | null xs   = Nothing
         | otherwise = Just $ "[" ++ concatMap show xs ++ "]"

instance Show ExternalID where
   show extID =
      case extID of
         System s   -> "SYSTEM " ++ doubleQuote s
         Public p s -> unwords ["PUBLIC", doubleQuote p, doubleQuote s]

instance Show DocTypeDecl where
   show decl =
      case decl of
         ElementDecl n c  -> "<!ELEMENT " ++ n ++ " " ++ show c ++ ">"
         AttListDecl n as -> "<!ATTLIST " ++ unwords (n:map showAttDef as) ++ ">"
         EntityDecl b n e ->
            let xs = ["%" | not b] ++ [n, showEntityDef e]
            in "<!ENTITY " ++ unwords xs ++ ">"
         NotationDecl n e ->
            let f s = "PUBLIC " ++ doubleQuote s
            in "<!NOTATION " ++ n ++ " " ++ either show f e ++ ">"
         DTDParameter r   -> show r
         DTDConditional c -> show c

instance Show ContentSpec where
   show cspec =
      case cspec of
         Empty -> "EMPTY"
         Any   -> "ANY"
         Mixed b ns ->
            let txt = intercalate "|" ("#PCDATA":ns)
            in parenthesize txt ++ (if b then "*" else "")
         Children cp -> show cp

instance Show CP where
   show cp =
      case cp of
         Choice xs      -> parenthesize (intercalate "|" (map show xs))
         Sequence xs    -> parenthesize (intercalate "," (map show xs))
         QuestionMark c -> show c ++ "?"
         Star c         -> show c ++ "*"
         Plus c         -> show c ++ "+"
         CPName n       -> n

instance Show AttType where
   show attType =
      case attType of
         IdType       -> "ID"
         IdRefType    -> "IDREF"
         IdRefsType   -> "IDREFS"
         EntityType   -> "ENTITY"
         EntitiesType -> "ENTITIES"
         NmTokenType  -> "NMTOKEN"
         NmTokensType -> "NMTOKENS"
         StringType   -> "CDATA"
         EnumerationType xs -> parenthesize (intercalate "|" xs)
         NotationType xs    -> "NOTATION " ++ parenthesize (intercalate "|" xs)

instance Show DefaultDecl where
   show defaultDecl =
      case defaultDecl of
         Required -> "#REQUIRED"
         Implied  -> "#IMPLIED"
         Value v  -> showAttValue v
         Fixed v  -> "#FIXED " ++ showAttValue v

instance Show Conditional where
   show conditional =
      case conditional of
         Include xs -> "<![INCLUDE[" ++ concatMap show xs ++ "]]>"
         Ignore _ -> "" -- ToDO undefined -- [String]

showXMLDecl :: XMLDoc -> String
showXMLDecl doc
   | isJust (versionInfo doc) = "<?xml " ++ unwords (catMaybes [s1,s2,s3]) ++ "?>"
   | otherwise = ""
 where
   s1 = fmap (\s -> "version=" ++ doubleQuote s) (versionInfo doc)
   s2 = fmap (\s -> "encoding=" ++ doubleQuote s) (encoding doc)
   s3 = fmap (\b -> "standalone=" ++ doubleQuote (if b then "yes" else "no")) (standalone doc)
-}
showOpenTag :: Bool -> Name -> Attributes -> String
showOpenTag close n as = "<" ++ unwords (n:map show as) ++
   (if close then "/>" else ">")

showCloseTag :: Name -> String
showCloseTag n = "</" ++ n ++ ">"

showAttValue :: AttValue -> String -- TODO: no double quotes allowed (should be escaped)
showAttValue = doubleQuote . concatMap (either f show)
 where
   f '"' = []
   f c   = [c]
{-
showEntityValue :: EntityValue -> String
showEntityValue = doubleQuote . concatMap (either f (either show show))
 where
   f '"' = []
   f c   = [c]

showAttDef :: AttDef -> String
showAttDef (s, tp, dd) = unwords [s, show tp, show dd]

showEntityDef :: EntityDef -> String
showEntityDef entityDef =
   case entityDef of
      Left ev -> showEntityValue ev
      Right (eid, ms) -> show eid ++ maybe "" (" NDATA "++) ms
-}
doubleQuote :: String -> String
doubleQuote s = "\"" ++ s ++ "\""
{-
parenthesize :: String -> String
parenthesize s = "(" ++ s ++ ")" -}