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
-- Collection of common operation on XML documents
--
-----------------------------------------------------------------------------
module Text.XML.Interface
   ( Element(..), Content, Attribute(..), Attributes
   , normalize, parseXML
   , children, findAttribute, findChild, getData
   ) where

import Control.Arrow
import Data.Char (chr, ord)
import Text.Parsing (parseSimple)
import Text.XML.Document (Name)
import Text.XML.Parser (document)
import Text.XML.Unicode (decoding)
import qualified Text.XML.Document as D

data Element = Element
   { name       :: Name
   , attributes :: Attributes
   , content    :: Content
   }

instance Show Element where
   show = show . extend

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
   refToContent (D.EntityRef s) = maybe [] id (lookup s entities)

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

findChild :: Monad m => String -> Element -> m Element
findChild s e =
   case filter ((==s) . name) (children e) of
      [a] -> return a
      _   -> fail $ "Child not found: " ++ show s

children :: Element -> [Element]
children e = [ c | Right c <- content e ]

getData :: Element -> String
getData e = concat [ s | Left s <- content e ]