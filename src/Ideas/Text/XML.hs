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
-- Collection of common operation on XML documents
--
-----------------------------------------------------------------------------

module Ideas.Text.XML
   ( -- * XML types
     XML, Name, Attributes, Attribute(..)
     -- * Parsing XML
   , parseXML, parseXMLFile
     -- * Building/constructing XML
   , BuildXML(..), XMLBuilder, makeXML
     -- * Pretty-printing XML
   , prettyXML, compactXML
     -- * Simple decoding queries
   , name, attributes, findAttribute, children, findChildren, findChild
   , getData, expecting
     -- * Decoding XML
   , decodeData, decodeAttribute, decodeChild, decodeFirstChild
     -- * Type classes for converting to/from XML
   , ToXML(..), builderXML, InXML(..)
     -- * Processing XML
   , foldXML, trimXML
     -- * Deprecated functions
   , content, emptyContent, fromBuilder
   ) where

import Control.Monad.State
import Data.Char (chr, ord, isSpace)
import Data.Foldable (toList)
import Data.List
import Data.Maybe
import Data.Semigroup as Sem
import Data.String
import Ideas.Text.XML.Document (escape, Name, prettyElement)
import Ideas.Text.XML.Parser (document)
import Ideas.Text.XML.Unicode
import Ideas.Utils.Decoding
import Ideas.Utils.Parsing (parseSimple)
import System.IO
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Ideas.Text.XML.Document as D

-------------------------------------------------------------------------------
-- XML types

-- invariants content: no two adjacent Lefts, no Left with empty string,
-- valid tag/attribute names
data XML = Tag
   { name       :: Name
   , attributes :: Attributes
   , content    :: [Either String XML]
   }
 deriving Eq

instance Show XML where
   show = compactXML

type Attributes = [Attribute]

data Attribute = Name := String
 deriving Eq

-------------------------------------------------------------------------------
-- Parsing XML

parseXML :: String -> Either String XML
parseXML input = do
   doc   <- parseSimple document input
   return (fromXMLDoc doc)

parseXMLFile :: FilePath -> IO XML
parseXMLFile file =
   withBinaryFile file ReadMode $
      hGetContents >=> either fail return . parseXML

fromXMLDoc :: D.XMLDoc -> XML
fromXMLDoc doc = fromElement (D.root doc)
 where
   fromElement (D.Element n as c) =
      makeXML n (fromAttributes as <> fromContent c)

   fromAttributes = mconcat . map fromAttribute

   fromAttribute (n D.:= v) =
      n .=. concatMap (either return refToString) v

   fromContent :: D.Content -> XMLBuilder
   fromContent = mconcat . map f
    where
      f :: D.XML -> XMLBuilder
      f (D.Tagged e)    = builder (fromElement e)
      f (D.CharData s)  = string s
      f (D.CDATA s)     = string s
      f (D.Reference r) = fromReference r

   refToString :: D.Reference -> String
   refToString (D.CharRef i)   = [chr i]
   refToString (D.EntityRef s) = maybe "" return (lookup s general)

   fromReference :: D.Reference -> XMLBuilder
   fromReference (D.CharRef i)   = char (chr i)
   fromReference (D.EntityRef s) = fromMaybe mempty (lookup s entities)

   entities :: [(String, XMLBuilder)]
   entities =
      [ (n, fromContent (snd ext)) | (n, ext) <- D.externals doc ] ++
      -- predefined entities
      [ (n, char c) | (n, c) <- general ]

   general :: [(String, Char)]
   general = [("lt",'<'), ("gt",'>'), ("amp",'&'), ("apos",'\''), ("quot",'"')]

-------------------------------------------------------------------------------
-- Building/constructing XML

infix 7 .=.

class (Sem.Semigroup a, Monoid a) => BuildXML a where
   (.=.)    :: String -> String -> a   -- attribute
   string   :: String -> a             -- (escaped) text
   builder  :: XML -> a                -- (named) xml element
   tag      :: String -> a -> a        -- tag (with content)
   -- functions with a default
   char     :: Char -> a
   text     :: Show s => s -> a -- escaped text with Show class
   element  :: String -> [a] -> a
   emptyTag :: String -> a
   -- implementations
   char c     = string [c]
   text       = string . show
   element s  = tag s . mconcat
   emptyTag s = tag s mempty

instance BuildXML a => BuildXML (Decoder env err s a) where
   n .=. s = pure (n .=. s)
   string  = pure . string
   builder = pure . builder
   tag     = fmap . tag

data XMLBuilder = BS (Seq.Seq Attribute) (Seq.Seq (Either String XML))

instance Sem.Semigroup XMLBuilder where
  BS as1 elts1 <> BS as2 elts2 = BS (as1 <> as2) (elts1 <> elts2)

instance Monoid XMLBuilder where
   mempty  = BS mempty mempty
   mappend = (<>)

instance BuildXML XMLBuilder where
   n .=. s  = nameCheck n $ BS (Seq.singleton (n := s)) mempty
   string s = BS mempty (if null s then mempty else Seq.singleton (Left s))
   builder  = BS mempty . Seq.singleton . Right
   tag n    = builder . uncurry (Tag n) . fromBS . nameCheck n

instance IsString XMLBuilder where
   fromString = string

makeXML :: String -> XMLBuilder -> XML
makeXML s = uncurry (Tag s) . fromBS . nameCheck s

nameCheck :: String -> a -> a
nameCheck s = if isName s then id else fail $ "Invalid name " ++ s

isName :: String -> Bool
isName []     = False
isName (x:xs) = (isLetter x || x `elem` "_:") && all isNameChar xs

isNameChar :: Char -> Bool
isNameChar c = any ($ c) [isLetter, isDigit, isCombiningChar, isExtender, (`elem` ".-_:")]

-- local helper: merge attributes, but preserve order
fromBS :: XMLBuilder -> (Attributes, [Either String XML])
fromBS (BS as elts) = (attrList, merge (toList elts))
 where
   attrMap = foldr add M.empty as
   add (k := v) = M.insertWith (\x y -> x ++ " " ++ y) k v
   attrList = nubBy eqKey (map make (toList as))
   make (k := _) = k := M.findWithDefault "" k attrMap
   eqKey (k1 := _) (k2 := _) = k1 == k2

   merge [] = []
   merge (Left x:Left y:rest)  = merge (Left (x++y):rest)
   merge (Left x:rest)  = Left x : merge rest
   merge (Right y:rest) = Right y : merge rest

-------------------------------------------------------------------------------
-- Pretty-printing XML

prettyXML :: XML -> String
prettyXML = show . prettyElement False . toElement

compactXML :: XML -> String
compactXML = show . prettyElement True . toElement

toElement :: XML -> D.Element
toElement = foldXML make mkAttribute mkString
 where
   make n as = D.Element n as . concatMap (either id (return . D.Tagged))

   mkAttribute :: Attribute -> D.Attribute
   mkAttribute (m := s) = (D.:=) m (map Left s)

   mkString :: String -> [D.XML]
   mkString [] = []
   mkString xs@(hd:tl)
      | null xs1  = D.Reference (D.CharRef (ord hd)) : mkString tl
      | otherwise = D.CharData xs1 : mkString xs2
    where
      (xs1, xs2) = break ((> 127) . ord) xs

-------------------------------------------------------------------------------
-- Simple decoding queries

findAttribute :: Monad m => String -> XML -> m String
findAttribute s (Tag _ as _) =
   case [ t | n := t <- as, s==n ] of
      [hd] -> return hd
      _    -> fail $ "Invalid attribute: " ++ show s

children :: XML -> [XML]
children e = [ c | Right c <- content e ]

findChildren :: String -> XML -> [XML]
findChildren s = filter ((==s) . name) . children

findChild :: Monad m => String -> XML -> m XML
findChild s e =
   case findChildren s e of
      []  -> fail $ "Child not found: " ++ show s
      [a] -> return a
      _   -> fail $ "Multiple children found: " ++ show s

getData :: XML -> String
getData e = concat [ s | Left s <- content e ]

expecting :: Monad m => String -> XML -> m ()
expecting s xml =
   unless (name xml == s) $ fail $ "Expecting element " ++ s ++ ", but found " ++ name xml

-------------------------------------------------------------------------------
-- Decoding XML

decodeData :: IsString err => Decoder env err XML String
decodeData = get >>= \xml ->
   case content xml of
      Left s:rest -> put xml {content = rest} >> return s
      _           -> fail "Could not find data"

decodeAttribute :: IsString err => String -> Decoder env err XML String
decodeAttribute s = get >>= \xml ->
   case break hasName (attributes xml) of
      (xs, (_ := val):ys) -> put xml {attributes = xs ++ ys } >> return val
      _ -> fail $ "Could not find attribute " ++ s
 where
   hasName (n := _) = n == s

decodeChild :: IsString err => Name -> Decoder env err XML a -> Decoder env err XML a
decodeChild s p = get >>= \xml ->
   case break hasName (content xml) of
      (xs, Right y:ys) -> do
         put y
         a <- p
         put xml { content = xs ++ ys }
         return a
      _ -> fail $ "Could not find child " ++ s
 where
   hasName = either (const False) ((==s) . name)

decodeFirstChild :: IsString err => Name -> Decoder env err XML a -> Decoder env err XML a
decodeFirstChild s p = get >>= \xml ->
   case content xml of
      Right y:ys | name y == s -> do
         put y
         a <- p
         put xml { content = ys }
         return a
      _ -> fail $ "Could not find first child " ++ s

-------------------------------------------------------------------------------
-- Type classes for converting to/from XML

class ToXML a where
   toXML     :: a -> XML
   listToXML :: [a] -> XML
   -- default definitions
   listToXML = makeXML "list" . mconcat . map builderXML

instance ToXML () where
   toXML _ = makeXML "Unit" mempty

instance ToXML a => ToXML (Maybe a) where
  toXML = maybe (makeXML "Nothing" mempty) toXML

builderXML :: (ToXML a, BuildXML b) => a -> b
builderXML = builder . toXML

class ToXML a => InXML a where
   fromXML     :: Monad m => XML -> m a
   listFromXML :: Monad m => XML -> m [a]
   listFromXML xml
      | name xml == "list" && null (attributes xml) =
           mapM fromXML (children xml)
      | otherwise = fail "expecting a list tag"

-------------------------------------------------------------------------------
-- Processing XML

foldXML :: (Name -> [a] -> [Either s e] -> e) -> (Attribute -> a) -> (String -> s) -> XML -> e
foldXML fe fa fs = rec
 where
   rec (Tag n as cs) = fe n (map fa as) (map (either (Left . fs) (Right . rec)) cs)

trimXML :: XML -> XML
trimXML = foldXML make fa (string . trim)
 where
   fa (n := s) = n .=. trim s

   make :: String -> [XMLBuilder] -> [Either XMLBuilder XML] -> XML
   make s as = makeXML s . mconcat . (as ++) . map (either id builder)

trim, trimLeft, trimRight :: String -> String
trim      = trimLeft . trimRight
trimLeft  = dropWhile isSpace
trimRight = reverse . trimLeft . reverse

-------------------------------------------------------------------------------
-- Deprecated functions

emptyContent :: XML -> Bool
emptyContent = null . content

fromBuilder :: XMLBuilder -> Maybe XML
fromBuilder m =
   case fromBS m of
      ([], [Right a]) -> Just a
      _               -> Nothing

-------------------------------------------------------------------------------
-- Tests

_runTests :: IO ()
_runTests = do
   forM_ [testDataP, testAttrP, testDataB, testAttrB] $ \f ->
      pp $ map f tests
   forM_ [mkPD, mkPA, mkBD, mkBA] $ \f ->
      pp $ map (testXML . f) tests
 where
   pp = putStrLn . map (\b -> if b then '.' else 'X')

   tests :: [String]
   tests =
      [ "input"
      , "&lt;&gt;&amp;&quot;&apos;"
      , "<>&'\""
      , "p & q' => p"
      , ""
      , "   "
      , "eerste \n\n derde regel"
      ]

   testDataP, testAttrP, testDataB, testAttrB :: String -> Bool
   testDataP s = let xml = mkPD s in getData xml == s
   testAttrP s = let xml = mkPA s in findAttribute "a" xml == Just s
   testDataB s = let xml = mkBD s in getData xml == s
   testAttrB s = let xml = mkBA s in findAttribute "a" xml == Just s

   testXML :: XML -> Bool
   testXML xml =
      case parseXML (compactXML xml) of
         Left msg -> error msg
         Right a  -> a == xml

   mkPD, mkPA, mkBD, mkBA :: String -> XML
   mkPD s = either error id $ parseXML $ "<a>" ++ escape s ++ "</a>"
   mkPA s = either error id $ parseXML $ "<t a='" ++ escape s ++ "'/>"
   mkBD s = makeXML "a" (string s)
   mkBA s = makeXML "t" ("a".=. s)