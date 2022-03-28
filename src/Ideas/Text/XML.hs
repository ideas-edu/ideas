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
     XML, Name, fromString, Attributes, Attribute(..)
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
   , ToXML(..), builderXML, InXML(..), DecoderXML
     -- * Processing XML
   , foldXML, trimXML
     -- * Deprecated functions
   , content, emptyContent, fromBuilder
   ) where

import Control.Arrow
import Control.Monad.State
import Data.Char (chr)
import Data.Maybe
import Data.String
import Ideas.Text.XML.Builder
import Ideas.Text.XML.Data
import Ideas.Text.XML.Document (escape, Name)
import Ideas.Text.XML.Parser (document)
import Ideas.Utils.Decoding
import Ideas.Utils.Parsing (parseSimple)
import System.IO
import qualified Ideas.Text.XML.Document as D

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
      show n .=. concatMap (either return refToString) v

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
   refToString (D.EntityRef n) = maybe "" return (lookup n general)

   fromReference :: D.Reference -> XMLBuilder
   fromReference (D.CharRef i)   = char (chr i)
   fromReference (D.EntityRef n) = fromMaybe mempty (lookup n entities)

   entities :: [(Name, XMLBuilder)]
   entities =
      [ (n, fromContent (snd ext)) | (n, ext) <- D.externals doc ] ++
      -- predefined entities
      [ (n, char c) | (n, c) <- general ]

   general :: [(Name, Char)]
   general = map (first fromString) 
      [("lt",'<'), ("gt",'>'), ("amp",'&'), ("apos",'\''), ("quot",'"')]

-------------------------------------------------------------------------------
-- Simple decoding queries

findAttribute :: Monad m => String -> XML -> m String
findAttribute s (Tag _ as _) =
   case [ t | n := t <- as, s == show n ] of
      [hd] -> return hd
      _    -> fail $ "Invalid attribute: " ++ show s

children :: HasContent a => a -> [XML]
children = rec . getContent 
 where
   rec a =
      case headIsString a of
         Just (_, rest) -> rec rest
         Nothing -> 
            case headIsXML a of
               Just (xml, rest) -> xml : rec rest
               Nothing -> []

findChildren :: String -> XML -> [XML]
findChildren s = filter ((==s) . show . name) . children

findChild :: Monad m => String -> XML -> m XML
findChild s e =
   case findChildren s e of
      []  -> fail $ "Child not found: " ++ show s
      [a] -> return a
      _   -> fail $ "Multiple children found: " ++ show s

getData :: HasContent a => a -> String
getData = rec . getContent
 where
   rec a = 
      case headIsString a of
         Just (s, rest) -> s ++ rec rest
         Nothing -> 
            case headIsXML a of
               Just (_, rest) -> rec rest
               Nothing -> []

expecting :: Monad m => String -> XML -> m ()
expecting s xml =
   unless (show (name xml) == s) $ fail $ "Expecting element " ++ s ++ ", but found " ++ show (name xml)

-------------------------------------------------------------------------------
-- Decoding XML

decodeData :: Decoder env String XML String
decodeData = get >>= \xml ->
   case headIsString xml of
      Just (s, rest) -> put rest >> return s
      _              -> throwError "Could not find data"

decodeAttribute :: String -> Decoder env String XML String
decodeAttribute s = get >>= \xml ->
   case break hasName (attributes xml) of
      (xs, (_ := val):ys) -> put xml {attributes = xs ++ ys } >> return val
      _ -> throwError $ "Could not find attribute " ++ s
 where
   hasName (n := _) = show n == s

decodeChild :: String -> Decoder env String XML a -> Decoder env String XML a
decodeChild s p = get >>= \xml ->
   case extractChild (fromString s) (content xml) of
      Just (y, rest) -> do
         put y
         a <- p
         put xml { content = rest }
         return a
      _ -> throwError $ "Could not find child " ++ s

-- local helper
extractChild :: Name -> Content -> Maybe (XML, Content)
extractChild n = rec
 where
   rec a = 
      case headIsXML a of
         Just (xml, rest) 
            | n == name xml -> Just (xml, rest)
            | otherwise -> add (xmlToContent xml) (rec rest)
         Nothing ->
            case headIsString a of
               Just (s, rest) -> add (fromString s) (rec rest)
               Nothing -> Nothing

   add a = fmap (fmap (a <>))

decodeFirstChild :: String -> Decoder env String XML a -> Decoder env String XML a
decodeFirstChild s p = get >>= \xml ->
   case headIsXML xml of
      Just (y, rest) | show (name y) == s -> do
         put y
         a <- p
         put rest
         return a
      _ -> throwError $ "Could not find first child " ++ s

-------------------------------------------------------------------------------
-- Type classes for converting to/from XML

class ToXML a where
   toXML     :: a -> XML
   listToXML :: [a] -> XML
   -- default definitions
   listToXML = makeXML (fromString "list") . mconcat . map builderXML

instance ToXML () where
   toXML _ = makeXML (fromString "Unit") mempty

instance ToXML a => ToXML (Maybe a) where
  toXML = maybe (makeXML (fromString "Nothing") mempty) toXML

builderXML :: (ToXML a, BuildXML b) => a -> b
builderXML = builder . toXML

type DecoderXML = Decoder () (Error XML) XMLBuilder

class ToXML a => InXML a where
   fromXML :: XML -> Maybe a
   --listFromXML :: XML -> Maybe [a] -- to be removed
   xmlDecoder :: DecoderXML a

   {-# MINIMAL fromXML | xmlDecoder #-}

   -- default definitions
  -- listFromXML xml
  --    | name xml == "list" && null (attributes xml) =
  --         mapM fromXML (children xml)
  --    | otherwise = fail "expecting a list tag"
   fromXML xml = either (const Nothing) Just $ evalDecoder xmlDecoder () (builder (trimXML xml))
   xmlDecoder = do
      b <- get 
      case fromBuilder b of
         Just xml -> maybe (errorStr "xml-decoder") return (fromXML xml) 
         _ -> errorStr "xml-decoder"

-------------------------------------------------------------------------------
-- Deprecated functions

emptyContent :: XML -> Bool
emptyContent = contentIsEmpty

fromBuilder :: XMLBuilder -> Maybe XML
fromBuilder m =
   case headIsXML m of
      Just (a, rest) | null (builderAttributes m) && contentIsEmpty rest 
          -> Just a
      _   -> Nothing

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
   mkBD s = makeXML (fromString "a") (string s)
   mkBA s = makeXML (fromString "t") ("a".=. s)