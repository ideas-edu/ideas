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
-- A datatype, parser, and pretty printer for XML documents. Re-exports
-- functions defined elsewhere.
--
-----------------------------------------------------------------------------

module Ideas.Text.XML
   ( XML, Attr, AttrList, Element(..), ToXML(..), builderXML, InXML(..)
   , XMLBuilder, isEmptyBuilder, makeXML
   , parseXML, parseXMLFile, compactXML, trimXML, findAttribute
   , children, Attribute(..), fromBuilder, findChild, findChildren, getData
   , BuildXML(..)
   , module Data.Monoid
   , (<>)
   , munless, mwhen
   ) where

import Control.Monad
import Data.Char
import Data.Foldable (toList)
import Data.List
import Data.Monoid hiding ((<>))
import Data.Semigroup as Sem
import Data.String
import Ideas.Text.XML.Interface
import System.IO
import qualified Data.Map as M
import qualified Data.Sequence as Seq

----------------------------------------------------------------
-- Datatype definitions

-- two helper types for attributes
type XML      = Element
type Attr     = Attribute  -- (String, String)
type AttrList = Attributes -- [Attr]

class ToXML a where
   toXML     :: a -> XML
   listToXML :: [a] -> XML
   -- default definitions
   listToXML = Element "list" [] . map (Right . toXML)

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

----------------------------------------------------------------
-- XML parser (a scanner and a XML tree constructor)

parseXMLFile :: FilePath -> IO XML
parseXMLFile file =
   withBinaryFile file ReadMode $
      hGetContents >=> either fail return . parseXML

trimXML :: XML -> XML
trimXML (Element n as xs) = Element n (map trimAttribute as) (trimContent xs)

trimContent :: Content -> Content
trimContent content = 
   case content of
      Left s:rest -> mk s ++ f rest
      _           -> f content
 where
   f [] = []
   f [Left s]     = mk s
   f (Left s:xs)  = mk s ++ f xs
   f (Right e:xs) = Right (trimXML e):f xs

   mk s = [ Left a | let a = trim s, not (null a) ]

trimAttribute :: Attribute -> Attribute
trimAttribute (n := s) = n := trim s

trim, trimLeft, trimRight :: String -> String
trim      = trimLeft . trimRight
trimLeft  = dropWhile isSpace
trimRight = reverse . trimLeft . reverse

----------------------------------------------------------------
-- XML builders

infix 7 .=.

class (Sem.Semigroup a, Monoid a) => BuildXML a where
   (.=.)     :: String -> String -> a   -- attribute
   unescaped :: String -> a             -- parsed character data (unescaped!)
   builder   :: Element -> a            -- (named) xml element
   tag       :: String -> a -> a        -- tag (with content)
   -- functions with a default
   string   :: String -> a -- escaped text
   text     :: Show s => s -> a -- escaped text with Show class
   element  :: String -> [a] -> a
   emptyTag :: String -> a
   -- implementations
   string     = unescaped -- . escape
   text       = string . show
   element s  = tag s . mconcat
   emptyTag s = tag s mempty

data XMLBuilder = BS (Seq.Seq Attr) (Seq.Seq (Either String Element))

isEmptyBuilder :: XMLBuilder -> Bool
isEmptyBuilder (BS as elts) = Seq.null as && Seq.null elts

-- local helper: merge attributes, but preserve order
fromBS :: XMLBuilder -> (AttrList, Content)
fromBS (BS as elts) = (attrList, toList elts)
 where
   attrMap = foldr add M.empty as
   add (k := v) = M.insertWith (\x y -> x ++ " " ++ y) k v
   attrList = nubBy eqKey (map make (toList as))
   make (k := _) = k := M.findWithDefault "" k attrMap
   eqKey (k1 := _) (k2 := _) = k1 == k2

instance Sem.Semigroup XMLBuilder where
  BS as1 elts1 <> BS as2 elts2 =
      BS (as1 <> as2) (elts1 <> elts2)

instance Monoid XMLBuilder where
   mempty  = BS mempty mempty
   mappend = (<>)

instance BuildXML XMLBuilder where
   n .=. s     = BS (Seq.singleton (n := s)) mempty
   unescaped s = BS mempty (if null s then mempty else Seq.singleton (Left s))
   builder     = BS mempty . Seq.singleton . Right
   tag s       = builder . uncurry (Element s) . fromBS

instance IsString XMLBuilder where
   fromString = string

makeXML :: String -> XMLBuilder -> XML
makeXML s = uncurry (Element s) . fromBS

mwhen :: Monoid a => Bool -> a -> a
mwhen True  a = a
mwhen False _ = mempty

munless :: Monoid a => Bool -> a -> a
munless = mwhen . not

escapeAttr :: String -> String
escapeAttr = concatMap f
 where
   f '<' = "&lt;"
   f '&' = "&amp;"
   f '"' = "&quot;"
   f c   = [c]

fromBuilder :: XMLBuilder -> Maybe Element
fromBuilder m =
   case fromBS m of
      ([], [Right a]) -> Just a
      _               -> Nothing

escape :: String -> String
escape = concatMap f
 where
   f '<'  = "&lt;"
   f '>'  = "&gt;"
   f '&'  = "&amp;"
   f '"'  = "&quot;"
   f '\'' = "&apos;"
   f '\n' = "&#10;" -- !!!!!!
   f c   = [c]

-------------------

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
         Right a  -> if a == xml then True else error $ show (a, xml)

   mkPD, mkPA, mkBD, mkBA :: String -> XML
   mkPD s = either error id $ parseXML $ "<a>" ++ escape s ++ "</a>"
   mkPA s = either error id $ parseXML $ "<t a='" ++ escape s ++ "'/>"
   mkBD s = makeXML "a" (string s)
   mkBA s = makeXML "t" ("a".=. s)