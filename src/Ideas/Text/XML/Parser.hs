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
-- A parser for XML documents, directly derived from the specification:
--
--    http://www.w3.org/TR/2006/REC-xml-20060816
--
-----------------------------------------------------------------------------

module Ideas.Text.XML.Parser (document, extParsedEnt, extSubset) where

import Control.Monad
import Data.Char (toUpper, ord)
import Data.List (foldl')
import Data.Maybe (catMaybes)
import Ideas.Text.XML.Document hiding (versionInfo, name, content)
import Ideas.Text.XML.Unicode
import Ideas.Utils.Parsing hiding (digit, letter, space)
import Prelude hiding (seq)
import qualified Ideas.Text.XML.Document as D

letter, digit, combiningChar, extender :: Parser Char
letter        = satisfy isLetter
digit         = satisfy isDigit
combiningChar = satisfy isCombiningChar
extender      = satisfy isExtender

-- combinators without lexing (no spaces are consumed)
parens, brackets, singleQuoted, doubleQuoted :: Parser a -> Parser a
parens       = between (char '(')  (char ')')
brackets     = between (char '[')  (char ']')
singleQuoted = between (char '\'') (char '\'')
doubleQuoted = between (char '"')  (char '"')

--------------------------------------------------
-- * 2 Documents

--------------------------------------------------
-- ** 2.1 Well-Formed XML Documents

-- [1]   	document	   ::=   	 prolog element Misc*
document :: Parser XMLDoc
document = do
   (mxml, mdtd) <- prolog
   rt <- element
   miscs
   let (ver, enc, sa) =
          case mxml of
             Just (a, b, c) -> (Just a, b, c)
             Nothing        -> (Nothing, Nothing, Nothing)
   return XMLDoc
      { D.versionInfo = ver
      , D.encoding    = enc
      , D.standalone  = sa
      , D.dtd         = mdtd
      , D.externals   = []
      , root        = rt
      }

--------------------------------------------------
-- ** 2.2 Characters

-- [2]   	Char	   ::=   	#x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
{-
char :: Parser Char
char = ranges xs <|> oneOf "\x9\xA\xD"
 where xs = [('\x20', '\xD7FF'), ('\xE000', '\xFFFD'), ('\x10000', '\x10FFFF')]
-}

--------------------------------------------------
-- ** 2.3 Common Syntactic Constructs

-- [3]   	S	   ::=   	(#x20 | #x9 | #xD | #xA)+
space :: Parser ()
space = void (many1 (oneOf "\x20\x9\xA\xD"))

mspace :: Parser () -- for S?
mspace = void (many (oneOf "\x20\x9\xA\xD"))

-- [4]   	NameChar	   ::=   	 Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar | Extender
nameChar :: Parser Char
nameChar = letter <|> digit <|> combiningChar <|> extender <|> oneOf ".-_:"

-- [5]   	Name	   ::=   	(Letter | '_' | ':') (NameChar)*
name :: Parser Name
name = do
   c  <- letter <|> oneOf "_:"
   cs <- many nameChar
   return (uncheckedName (c:cs))

spacedName :: Parser Name
spacedName = space *> name <* space

{-
-- [6]   	Names	   ::=   	 Name (#x20 Name)*
names :: Parser [String]
names = sepBy1 name (char '\x20')
-}

-- [7]   	Nmtoken	   ::=   	(NameChar)+
nmtoken :: Parser String
nmtoken = many1 nameChar

{-
-- [8]   	Nmtokens	   ::=   	 Nmtoken (#x20 Nmtoken)*
nmtokens :: Parser [String]
nmtokens = sepBy1 nmtoken (char '\x20')
-}

-- [9]   	EntityValue	   ::=   	'"' ([^%&"] | PEReference | Reference)* '"'
--                           |  "'" ([^%&'] | PEReference | Reference)* "'"
entityValue :: Parser EntityValue
entityValue = doubleQuoted (p "%&\"") <|> singleQuoted (p "%&'")
 where
   p s =  many (fmap Left (noneOf s)
      <|> fmap Right (fmap Left peReference <|> fmap Right reference))

-- [10]   	AttValue	   ::=   	'"' ([^<&"] | Reference)* '"'
--                           |  "'" ([^<&'] | Reference)* "'"
attValue :: Parser AttValue
attValue = doubleQuoted (p "<&\"") <|> singleQuoted (p "<&'")
 where p s = many (fmap Left (noneOf s) <|> fmap Right reference)

-- [11]   	SystemLiteral	   ::=   	('"' [^"]* '"') | ("'" [^']* "'")
systemLiteral :: Parser String
systemLiteral = doubleQuoted (p "\"") <|> singleQuoted (p "'")
 where
   p :: String -> Parser String
   p s = many (noneOf s)

-- [12]   	PubidLiteral	   ::=   	'"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
pubidLiteral :: Parser String
pubidLiteral = doubleQuoted (many (pubidChar True)) <|> singleQuoted (many (pubidChar False))

-- [13]   	PubidChar	   ::=   	#x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
pubidChar :: Bool -> Parser Char
pubidChar withSingleQuote =
   ranges xs <|> oneOf "\x20\xD\xA-()+,./:=?;!*#@$_%" <|> singleQuote
 where
   xs = [('a', 'z'), ('A', 'Z'), ('0', '9')]
   singleQuote
      | withSingleQuote = char '\'' >> return '\''
      | otherwise       = fail "pubidChar"

--------------------------------------------------
-- ** 2.4 Character Data and Markup

-- [14]   	CharData	   ::=   	[^<&]* - ([^<&]* ']]>' [^<&]*)
charData :: Parser String -- This implementation is too liberal since it allows "]]>"
charData = stopOn ["<", "&", "]]>"]

--------------------------------------------------
-- ** 2.5 Comments

-- [15]   	Comment	   ::=   	'<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
comment :: Parser String
comment = between (string "<!--") (string "-->") (stopOn ["--"])

--------------------------------------------------
-- ** 2.6 Processing Instructions

-- [16]   	PI	   ::=   	'<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
pInstr :: Parser String
pInstr = between (string "<?") (string "?>") p
 where
   p = piTarget >> option "" (space >> stopOn ["?>"])

-- [17]   	PITarget	   ::=   	 Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
piTarget :: Parser ()
piTarget = do
   n <- name
   when (map toUpper (show n) == "XML") $ fail "XML in piTarget"

--------------------------------------------------
-- ** 2.7 CDATA Sections

-- [18]   	CDSect	   ::=   	 CDStart CData CDEnd
-- [19]   	CDStart	   ::=   	'<![CDATA['
-- [20]   	CData	   ::=   	(Char* - (Char* ']]>' Char*))
-- [21]   	CDEnd	   ::=   	']]>'
cdSect :: Parser XML
cdSect = between (string "<![CDATA[") (string "]]>") p
 where
   p = do
      s <- stopOn ["]]>"]
      return (CDATA s)

--------------------------------------------------
-- ** 2.8 Prolog and Document Type Declaration

type XMLDecl = (String, Maybe String, Maybe Bool)

-- [22]   	prolog	   ::=   	 XMLDecl? Misc* (doctypedecl Misc*)?
prolog :: Parser (Maybe XMLDecl, Maybe DTD)
prolog = do
   ma <- optionMaybe (try xmlDecl)
   miscs
   mb <- optionMaybe $ try $ do
      mb <- doctypedecl
      miscs
      return mb
   return (ma, mb)

-- [23]   	XMLDecl	   ::=   	'<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
xmlDecl :: Parser XMLDecl
xmlDecl = do
   skip (string "<?xml")
   x <- versionInfo
   y <- optionMaybe (try encodingDecl)
   z <- optionMaybe (try sdDecl)
   mspace
   skip (string "?>")
   return (x, y, z)

-- [24]   	VersionInfo	   ::=   	 S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
versionInfo :: Parser String
versionInfo = space >> string "version" >> eq >> p
 where p = singleQuoted versionNum <|> doubleQuoted versionNum

-- [25]   	Eq	   ::=   	 S? '=' S?
eq :: Parser ()
eq = skip (mspace >> char '=' >> mspace)

-- [26]   	VersionNum	   ::=   	'1.0'
versionNum :: Parser String
versionNum = do
   skip (string "1.0")
   return "1.0"

-- [27]   	Misc	   ::=   	 Comment | PI | S
misc :: Parser ()
misc = try (skip comment) <|> try (skip pInstr) <|> skip space

miscs :: Parser ()
miscs = skip (many misc)

-- [28]   	doctypedecl	   ::=   	'<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
doctypedecl :: Parser DTD
doctypedecl = do
   skip (string "<!DOCTYPE")
   space
   x <- name
   y <- optionMaybe (try (space >> externalID))
   mspace
   z <- option [] $ do
      z <- brackets intSubset
      mspace
      return z
   skip (char '>')
   return (DTD x y z)

-- [28a]   	DeclSep	   ::=   	 PEReference | S
declSep :: Parser (Maybe DocTypeDecl)
declSep =  fmap (Just . DTDParameter) peReference
       <|> (space >> return Nothing)

-- [28b]   	intSubset	   ::=   	(markupdecl | DeclSep)*
intSubset :: Parser [DocTypeDecl]
intSubset = fmap catMaybes (many (markupdecl <|> declSep))

-- [29]   	markupdecl	   ::=   	 elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
markupdecl :: Parser (Maybe DocTypeDecl)
markupdecl =  fmap Just (choice (map try list))
          <|> ((try pInstr <|> comment) >> return Nothing)
 where
   list = [elementdecl, attlistDecl, entityDecl, notationDecl]

-- [30]   	extSubset	   ::=   	 TextDecl? extSubsetDecl
extSubset :: Parser (Maybe TextDecl, [DocTypeDecl])
extSubset = do
   m <- optionMaybe textDecl
   e <- extSubsetDecl
   return (m, e)

-- [31]   	extSubsetDecl	   ::=   	( markupdecl | conditionalSect | DeclSep)*
extSubsetDecl :: Parser [DocTypeDecl]
extSubsetDecl = fmap catMaybes (many (choice [markupdecl, fmap (Just . DTDConditional) conditionalSect, declSep]))

--------------------------------------------------
-- ** 2.9 Standalone Document Declaration
-- [32]   	SDDecl	   ::=   	 S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
sdDecl :: Parser Bool
sdDecl = space >> string "standalone" >> eq >> (singleQuoted bool <|> doubleQuoted bool)
 where bool =  (string "yes" >> return True)
           <|> (string "no"  >> return False)

--------------------------------------------------
-- ** 2.10 White Space Handling

--------------------------------------------------
-- * 3 Logical Structures

-- [39]   	element	   ::=   	 EmptyElemTag | STag content ETag
element :: Parser Element
element = do
   (s1, as, closed) <- sTag
   if closed
     then return (Element s1 as [])
     else do
       c  <- content
       s2 <- eTag
       when (s1/=s2) $ fail "WFC: element"
       return (Element s1 as c)

--------------------------------------------------
-- ** 3.1 Start-Tags, End-Tags, and Empty-Element Tags

-- [40]   	STag	   ::=   	'<' Name (S Attribute)* S? '>'
-- [44]   	EmptyElemTag	   ::=   	'<' Name (S Attribute)* S? '/>'
-- The boolean indicates whether the tag was closed immediately (an EmptyElemTag)
sTag :: Parser (Name, Attributes, Bool)
sTag = do
   skip (char '<')
   n  <- name
   as <- many (try (space >> attribute))
   mspace
   b  <- (char '>'  >> return False) <|>
         (string "/>" >> return True)
   return (n, as, b)

-- [41]   	Attribute	   ::=   	NSAttName Eq AttValue
--        | Name Eq AttValue
attribute :: Parser Attribute
attribute = do
   n <- name
   eq
   a <- attValue
   return (n := a)

-- [42]   	ETag	   ::=   	'</' Name S? '>'
eTag :: Parser Name
eTag = do
   skip (string "</")
   n <- name
   mspace
   skip (char '>')
   return n

-- [43]   	content	   ::=   	 CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
-- Note: since CharData accepts epsilon, there is no need to make it optional
content :: Parser Content
content = chainr1 (fmap g charData) (fmap f ps)
 where
   f ma l r = l ++ maybe [] return ma ++ r
   g s = [ CharData s | not (null s) ]
   ps  = try (fmap Just (choice (map try [fmap Tagged element, fmap Reference reference, cdSect]))
      <|> ((try pInstr <|> comment) >> return Nothing))

--------------------------------------------------
-- ** 3.2 Element Type Declarations

-- [45]   	elementdecl	   ::=   	'<!ELEMENT' S Name S contentspec S? '>'
elementdecl :: Parser DocTypeDecl
elementdecl = do
   skip (string "<!ELEMENT")
   n <- spacedName
   cs <- contentspec
   mspace
   skip (char '>')
   return (ElementDecl n cs)

-- [46]   	contentspec	   ::=   	'EMPTY' | 'ANY' | Mixed | children
contentspec :: Parser ContentSpec
contentspec = choice
   [ string "EMPTY" >> return Empty
   , string "ANY"   >> return Any
   , try mixed
   , children
   ]

-- [47]   	children	   ::=   	(choice | seq) ('?' | '*' | '+')?
children :: Parser ContentSpec
children = do
   a <- try cpChoice <|> cpSeq
   f <- option id multi
   return (Children (f a))

multi :: Parser (CP -> CP)
multi =  (char '?' >> return QuestionMark)
     <|> (char '*' >> return Star)
     <|> (char '+' >> return Plus)

-- [48]   	cp	   ::=   	(Name | choice | seq) ('?' | '*' | '+')?
cp :: Parser CP
cp = do
   a <- fmap CPName name <|> try cpChoice <|> cpSeq
   f <- option id multi
   return (f a)

-- [49]   	choice	   ::=   	'(' S? cp ( S? '|' S? cp )+ S? ')'
cpChoice :: Parser CP
cpChoice = parens $ do
   mspace
   x  <- cp
   xs <- many1 (try (mspace >> char '|' >> mspace >> cp))
   mspace
   return (Choice (x:xs))

-- [50]   	seq	   ::=   	'(' S? cp ( S? ',' S? cp )* S? ')'
cpSeq :: Parser CP
cpSeq = parens $ do
   mspace
   x  <- cp
   xs <- many (try (mspace >> char ',' >> mspace >> cp))
   mspace
   return (Sequence (x:xs))

-- [51]   	Mixed	   ::=   	'(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'
--                  | '(' S? '#PCDATA' S? ')'
mixed :: Parser ContentSpec
mixed = char '(' >> mspace >> string "#PCDATA" >> (rest1 <|> rest2)
 where
   p = mspace >> char '|' >> mspace >> name
   rest1 = try $ do
       xs <- many (try p)
       mspace
       skip (string ")*")
       return (Mixed True xs)
   rest2 = mspace >> char ')' >> return (Mixed False [])

--------------------------------------------------
-- ** 3.3 Attribute-List Declarations

-- [52]   	AttlistDecl	   ::=   	'<!ATTLIST' S Name AttDef* S? '>'
attlistDecl :: Parser DocTypeDecl
attlistDecl = do
   skip (string "<!ATTLIST")
   space
   n  <- name
   ds <- many (try attDef)
   mspace
   skip (char '>')
   return (AttListDecl n ds)

-- [53]   	AttDef	   ::=   	 S Name S AttType S DefaultDecl
attDef :: Parser AttDef
attDef = do
   n  <- spacedName
   tp <- attType
   space
   dd <- defaultDecl
   return (n, tp, dd)

-- [54]   	AttType	   ::=   	 StringType | TokenizedType | EnumeratedType
attType :: Parser AttType
attType = stringType <|> tokenizedType <|> enumeratedType

-- [55]   	StringType	   ::=   	'CDATA'
stringType :: Parser AttType
stringType = string "CDATA" >> return StringType

-- [56]   	TokenizedType	   ::=   	'ID' | 'IDREF' | 'IDREFS' | 'ENTITY' | 'ENTITIES' | 'NMTOKEN' | 'NMTOKENS'
tokenizedType :: Parser AttType
tokenizedType = choice (map f xs)
 where
   f (tp, s) = try (string s) >> return tp
   xs = [ (IdRefsType, "IDREFS"), (IdRefType, "IDREF"), (IdType, "ID"), (EntityType, "ENTITY")
        , (EntitiesType, "ENTITIES"), (NmTokensType, "NMTOKENS"), (NmTokenType, "NMTOKEN")
        ]

-- [57]   	EnumeratedType	   ::=   	 NotationType | Enumeration
enumeratedType :: Parser AttType
enumeratedType = notationType <|> enumeration

-- [58]   	NotationType	   ::=   	'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
notationType :: Parser AttType
notationType = string "NOTATION" >> space >> parens p
 where
   p = do
      mspace
      n  <- name
      ns <- many (try (mspace >> char '|' >> mspace >> name))
      mspace
      return (NotationType (n:ns))

-- [59]   	Enumeration	   ::=   	'(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
enumeration :: Parser AttType
enumeration = parens $ do
   mspace
   x  <- nmtoken
   xs <- many (try (mspace >> char '|' >> mspace >> nmtoken))
   mspace
   return (EnumerationType (x:xs))

-- [60]   	DefaultDecl	   ::=   	'#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue)
defaultDecl :: Parser DefaultDecl
defaultDecl =  try (string "#REQUIRED" >> return Required)
           <|> try (string "#IMPLIED"  >> return Implied)
           <|> do f <- option Value (string "#FIXED" >> space >> return Fixed)
                  a <- attValue
                  return (f a)

--------------------------------------------------
-- ** 3.4 Conditional Sections

-- [61]   	conditionalSect	   ::=   	 includeSect | ignoreSect
conditionalSect :: Parser Conditional
conditionalSect = try includeSect <|> ignoreSect

-- [62]   	includeSect	   ::=   	'<![' S? 'INCLUDE' S? '[' extSubsetDecl ']]>'
includeSect :: Parser Conditional
includeSect = do
   skip (string "<![")
   mspace
   skip (string "INCLUDE")
   mspace
   skip (char '[')
   ds <- extSubsetDecl
   skip (string "]]>")
   return (Include ds)

-- [63]   	ignoreSect	   ::=   	'<![' S? 'IGNORE' S? '[' ignoreSectContents* ']]>'
ignoreSect :: Parser Conditional
ignoreSect = do
   skip (string "<![")
   mspace
   skip (string "IGNORE")
   mspace
   skip (char '[')
   xss <- many ignoreSectContents
   skip (string "]]>")
   return (Ignore (concat xss))

-- [64]   	ignoreSectContents	   ::=   	 Ignore ('<![' ignoreSectContents ']]>' Ignore)*
ignoreSectContents :: Parser [String]
ignoreSectContents =
   do x   <- ignore
      xss <- many $ do
         skip (string "<![")
         ys <- ignoreSectContents
         skip (string "]]>")
         y  <- ignore
         return (ys++[y])
      return (x:concat xss)

-- [65]   	Ignore	   ::=   	 Char* - (Char* ('<![' | ']]>') Char*)
ignore :: Parser String
ignore = stopOn ["<![", "]]>"]

--------------------------------------------------
-- * 4 Physical Structures

--------------------------------------------------
-- ** 4.1 Character and Entity References

-- [66]   	CharRef	   ::=   	'&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';'
charRef :: Parser Reference
charRef = do
   skip (string "&#")
   n <- p <|> (char 'x' >> q)
   skip (char ';')
   return (CharRef n)
 where
   p = fmap (foldl' (\a b -> a*10+ord b-48) 0) (many1 ('0' <..> '9'))
   q = fmap hexa (many1 (ranges [('0', '9'), ('a', 'f'), ('A', 'F')]))

hexa :: String -> Int
hexa = rec 0
 where
   rec n []     = n
   rec n (x:xs) = rec (16*n + ord x - correct) xs
    where
      correct
         | x <= '9'  = ord '0'
         | x <= 'F'  = ord 'A' - 10
         | otherwise = ord 'a' - 10

-- [67]   	Reference	   ::=   	 EntityRef | CharRef
reference :: Parser Reference
reference = try entityRef <|> charRef

-- [68]   	EntityRef	   ::=   	'&' Name ';'
entityRef :: Parser Reference
entityRef = between (char '&') (char ';') (fmap EntityRef name)

-- [69]   	PEReference	   ::=   	'%' Name ';'
peReference :: Parser Parameter
peReference = between (char '%') (char ';') (fmap Parameter name)

--------------------------------------------------
-- ** 4.2 Entity Declarations

-- [70]   	EntityDecl	   ::=   	 GEDecl | PEDecl
entityDecl :: Parser DocTypeDecl
entityDecl = try geDecl <|> peDecl

-- [71]   	GEDecl	   ::=   	'<!ENTITY' S Name S EntityDef S? '>'
geDecl :: Parser DocTypeDecl
geDecl = do
   skip (string "<!ENTITY")
   n  <- spacedName
   ed <- entityDef
   mspace
   skip (char '>')
   return (EntityDecl True n ed)

-- [72]   	PEDecl	   ::=   	'<!ENTITY' S '%' S Name S PEDef S? '>'
peDecl :: Parser DocTypeDecl
peDecl = do
   skip (string "<!ENTITY")
   space
   skip (char '%')
   n <- spacedName
   e <- peDef
   mspace
   skip (char '>')
   return (EntityDecl False n (either Left (\a -> Right (a, Nothing)) e))

-- [73]   	EntityDef	   ::=   	 EntityValue | (ExternalID NDataDecl?)
entityDef :: Parser EntityDef
entityDef = fmap Left entityValue <|> do
   e  <- externalID
   ms <- optionMaybe (try nDataDecl)
   return (Right (e, ms))

-- [74]   	PEDef	   ::=   	 EntityValue | ExternalID
peDef :: Parser (Either EntityValue ExternalID)
peDef = fmap Left entityValue <|> fmap Right externalID

-- [75]   	ExternalID	   ::=   	'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
externalID :: Parser ExternalID
externalID =  (string "SYSTEM" >> space >> fmap System systemLiteral) <|> do
   skip (string "PUBLIC")
   space
   x <- pubidLiteral
   space
   y <- systemLiteral
   return (Public x y)

-- [76]   	NDataDecl	   ::=   	 S 'NDATA' S Name
nDataDecl :: Parser Name
nDataDecl = space >> string "NDATA" >> space >> name

--------------------------------------------------
-- ** 4.3 Parsed Entities

-- [77]   	TextDecl	   ::=   	'<?xml' VersionInfo? EncodingDecl S? '?>'

textDecl :: Parser TextDecl
textDecl = do
   skip (string "<?xml")
   v <- optionMaybe versionInfo
   e <- encodingDecl
   mspace
   skip (string "?>")
   return (v, e)

-- [78]   	extParsedEnt	   ::=   	 TextDecl? content
extParsedEnt :: Parser (Maybe TextDecl, Content)
extParsedEnt = do
   td <- optionMaybe (try textDecl)
   c  <- content
   return (td, c)

-- [80]   	EncodingDecl	   ::=   	 S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
encodingDecl :: Parser String
encodingDecl = space >> string "encoding" >> eq >>
   (singleQuoted encName <|> doubleQuoted encName)

-- [81]   	EncName	   ::=   	[A-Za-z] ([A-Za-z0-9._] | '-')*
encName :: Parser String
encName = do
   x  <- ranges [('A', 'Z'), ('a', 'z')]
   xs <- many (ranges [('A', 'Z'), ('a', 'z'), ('0', '9')] <|> oneOf "._-")
   return (x:xs)

--------------------------------------------------
-- ** 4.7 Notation Declarations

-- [82]   	NotationDecl	   ::=   	'<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
notationDecl :: Parser DocTypeDecl
notationDecl = do
   skip (string "<!NOTATION")
   n <- spacedName
   e <- fmap Left (try externalID) <|> fmap Right publicID
   mspace
   skip (char '>')
   return (NotationDecl n e)

-- [83]   	PublicID	   ::=   	'PUBLIC' S PubidLiteral
publicID :: Parser PublicID
publicID = string "PUBLIC" >> space >> pubidLiteral