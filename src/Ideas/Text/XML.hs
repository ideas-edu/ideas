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
   , ToXML(..), builderXML, InXML(..), DecoderXML
     -- * Processing XML
   , foldXML, trimXML
     -- * Deprecated functions
   , content, emptyContent, fromBuilder
   ) where

import Control.Monad.State
import Data.Char (chr)
import Data.Maybe
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

decodeData :: Decoder env String XML String
decodeData = get >>= \xml ->
   case content xml of
      Left s:rest -> put xml {content = rest} >> return s
      _           -> throwError "Could not find data"

decodeAttribute :: String -> Decoder env String XML String
decodeAttribute s = get >>= \xml ->
   case break hasName (attributes xml) of
      (xs, (_ := val):ys) -> put xml {attributes = xs ++ ys } >> return val
      _ -> throwError $ "Could not find attribute " ++ s
 where
   hasName (n := _) = n == s

decodeChild :: Name -> Decoder env String XML a -> Decoder env String XML a
decodeChild s p = get >>= \xml ->
   case break hasName (content xml) of
      (xs, Right y:ys) -> do
         put y
         a <- p
         put xml { content = xs ++ ys }
         return a
      _ -> throwError $ "Could not find child " ++ s
 where
   hasName = either (const False) ((==s) . name)

decodeFirstChild :: Name -> Decoder env String XML a -> Decoder env String XML a
decodeFirstChild s p = get >>= \xml ->
   case content xml of
      Right y:ys | name y == s -> do
         put y
         a <- p
         put xml { content = ys }
         return a
      _ -> throwError $ "Could not find first child " ++ s

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
      case fromBS b of
         ([], [Right xml]) -> maybe (errorStr "xml-decoder") return (fromXML xml) 
         _ -> errorStr "xml-decoder"

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