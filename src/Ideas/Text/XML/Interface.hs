-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
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

module Ideas.Text.XML.Interface
   ( Element(..), Content, Attribute(..), Attributes
   , normalize, parseXML, compactXML
   , children, findAttribute, findChildren, findChild, getData
   ) where

import Control.Arrow
import Data.Char (chr, ord)
import Data.Maybe
import Ideas.Text.Parsing (parseSimple)
import Ideas.Text.XML.Document (Name, prettyElement)
import Ideas.Text.XML.Parser (document)
import Ideas.Text.XML.Unicode (decoding)
import qualified Ideas.Text.XML.Document as D

data Element = Element
   { name       :: Name
   , attributes :: Attributes
   , content    :: Content
   }

instance Show Element where
   show = show . extend

compactXML :: Element -> String
compactXML = show . prettyElement True . extend

type Content = [Either String Element]

type Attributes = [Attribute]
data Attribute = Name := String

normalize :: D.XMLDoc -> Element
normalize doc = toElement (D.root doc)
 where
   toElement :: D.Element -> Element
   toElement (D.Element n as c) =
      Element n (map toAttribute as) (toContent c)

   toAttribute :: D.Attribute -> Attribute
   toAttribute (n D.:= v) =
      n := concatMap (either return refToString) v

   toContent :: D.Content -> Content
   toContent = merge . concatMap f
    where
      f :: D.XML -> Content
      f (D.Tagged e)    = [Right (toElement e)]
      f (D.CharData s)  = [Left s]
      f (D.CDATA s)     = [Left s]
      f (D.Reference r) = refToContent r

   refToString :: D.Reference -> String
   refToString (D.CharRef i)   = [chr i]
   refToString (D.EntityRef s) = maybe "" return (lookup s general)

   refToContent :: D.Reference -> Content
   refToContent (D.CharRef i)   = [Left [chr i]]
   refToContent (D.EntityRef s) = fromMaybe [] (lookup s entities)

   entities :: [(String, Content)]
   entities =
      [ (n, toContent (snd ext)) | (n, ext) <- D.externals doc ] ++
      -- predefined entities
      map (second (return . Left . return)) general

   general :: [(String, Char)]
   general = [("lt",'<'), ("gt",'>'), ("amp",'&'), ("apos",'\''), ("quot",'"')]

   merge :: Content -> Content
   merge (Left s:Left t:rest) = merge (Left (s++t) : rest)
   merge (x:xs) = x:merge xs
   merge []     = []

extend :: Element -> D.Element
extend (Element n as c) =
   D.Element n (map toAttribute as) (concatMap toXML c)
 where
   toAttribute :: Attribute -> D.Attribute
   toAttribute (m := s) = (D.:=) m (map Left s)

   toXML :: Either String Element -> [D.XML]
   toXML = either fromString (return . D.Tagged . extend)

   fromString :: String -> [D.XML]
   fromString [] = []
   fromString xs@(hd:tl)
      | null xs1  = D.Reference (D.CharRef (ord hd)) : fromString tl
      | otherwise = D.CharData xs1 : fromString xs2
    where
      (xs1, xs2) = break ((> 127) . ord) xs

-----------------------------------------------------

parseXML :: String -> Either String Element
parseXML xs = do
   input <- decoding xs
   doc   <- parseSimple document input
   return (normalize doc)

-----------------------------------------------------

findAttribute :: Monad m => String -> Element -> m String
findAttribute s (Element _ as _) =
   case [ t | n := t <- as, s==n ] of
      [hd] -> return hd
      _    -> fail $ "Invalid attribute: " ++ show s

findChildren :: String -> Element -> [Element]
findChildren s = filter ((==s) . name) . children

findChild :: Monad m => String -> Element -> m Element
findChild s e =
   case findChildren s e of
      []  -> fail $ "Child not found: " ++ show s
      [a] -> return a
      _   -> fail $ "Multiple children found: " ++ show s

children :: Element -> [Element]
children e = [ c | Right c <- content e ]

getData :: Element -> String
getData e = concat [ s | Left s <- content e ]