{-# LANGUAGE FlexibleInstances #-}
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
-- A datatype, parser, and pretty printer for XML documents. Re-exports
-- functions defined elsewhere.
--
-----------------------------------------------------------------------------
module Ideas.Text.XML
   ( XML, Attr, AttrList, InXML(..), Element(..)
   , XMLBuilder, XMLBuilderM, makeXML, buildXML, text, unescaped, element, tag, attribute
   , parseXML, showXML, compactXML, (.=.), findAttribute, updateLast
   , children, Attribute(..), builder, fromBuilder, findChild, getData
   ) where

import Control.Arrow
import Control.Monad
import Data.Char
import Data.Foldable (toList)
import Data.Monoid
import qualified Data.Sequence as Seq
import Ideas.Text.XML.Interface hiding (parseXML)
import qualified Ideas.Text.XML.Interface as I

----------------------------------------------------------------
-- Datatype definitions

-- two helper types for attributes
type XML      = Element
type Attr     = Attribute  -- (String, String)
type AttrList = Attributes -- [Attr]

class InXML a where
   toXML       :: a -> XML
   listToXML   :: [a] -> XML
   fromXML     :: Monad m => XML -> m a
   listFromXML :: Monad m => XML -> m [a]
   -- default definitions
   listToXML = Element "list" [] . map (Right . toXML)
   listFromXML xml
      | name xml == "list" && null (attributes xml) =
           mapM fromXML (children xml)
      | otherwise = fail "expecting a list tag"

----------------------------------------------------------------
-- XML parser (a scanner and a XML tree constructor)

parseXML :: String -> Either String XML
parseXML input = do
   xml <- I.parseXML input
   return (ignoreLayout xml)

ignoreLayout :: XML -> XML
ignoreLayout (Element n as xs) =
   let f = either (Left . trim) (Right . ignoreLayout)
   in Element n as (map f xs)

indentXML :: XML -> XML
indentXML = rec 0
 where
   rec i (Element n as xs) =
      let ipl  = i+2
          cd j = Left ('\n' : replicate j ' ')
          f    = either (\x -> [cd ipl, Left x]) (\x -> [cd ipl, Right (rec ipl x)])
          body | null xs   = xs
               | otherwise = concatMap f xs ++ [cd i]
      in Element n as body

showXML :: XML -> String
showXML = (++"\n") . show . indentXML . ignoreLayout

compactXML :: XML -> String
compactXML = show . ignoreLayout

----------------------------------------------------------------
-- Monadic XML builder

data BuilderState = BS (Seq.Seq Attr) (Seq.Seq (Either String Element))

instance Monoid BuilderState where
   mempty = BS mempty mempty
   mappend (BS as1 elts1) (BS as2 elts2) =
      BS (as1 <> as2) (elts1 <> elts2)

attrBS :: Attr -> BuilderState
attrBS a = BS (Seq.singleton a) mempty

elementBS :: Element -> BuilderState
elementBS e = BS mempty (Seq.singleton (Right e))

textBS :: String -> BuilderState
textBS s = BS mempty (Seq.singleton (Left s))

fromBS :: BuilderState -> (AttrList, Content)
fromBS (BS as elts) = (toList as, toList elts)

type XMLBuilder = XMLBuilderM ()

newtype XMLBuilderM a = XB 
   { fromXB :: BuilderState -> Either String (BuilderState, a) 
   }

instance Monoid a => Monoid (XMLBuilderM a) where
   mempty  = return mempty
   mappend = (>>)

instance Monad XMLBuilderM where
   return a = XB $ \bs -> Right (bs, a)
   fail s   = XB $ \_  -> Left s
   m >>= f  = XB $ \bs -> 
      case fromXB m bs of
         Left s       -> Left s
         Right (b, a) -> fromXB (f a) b

instance MonadPlus XMLBuilderM where
   mzero = fail "XMLBuilderM: mzero"
   mplus m1 m2 = XB $ \bs -> 
      case fromXB m1 bs of
         Left _   -> fromXB m2 bs
         Right ok -> Right ok

runXMLBuilder :: XMLBuilder -> Either String (AttrList, Content)
runXMLBuilder m = 
   fmap (fromBS . fst) (fromXB m mempty)

modify :: (BuilderState -> BuilderState) -> XMLBuilder
modify f = XB (\bs -> Right (f bs, ()))

append :: BuilderState -> XMLBuilder
append bs = modify (<> bs)

makeXML :: String -> XMLBuilder -> XML
makeXML s m = either error id (buildXML s m)

buildXML :: String -> XMLBuilder -> Either String XML
buildXML s m =
   case runXMLBuilder m of
      Left msg         -> Left msg
      Right (as, elts) -> Right (Element s as elts)

updateLast :: (Element -> Element) -> XMLBuilder -> XMLBuilder
updateLast f m = XB $ \bs -> fmap (first change) (fromXB m bs)
 where
   change (BS as elts) =
      case Seq.viewr elts of 
         rest Seq.:> b -> BS as (rest Seq.|> fmap f b)
         Seq.EmptyR    -> BS as elts

text :: String -> XMLBuilder
text = unescaped . escape

-- Should be used with care: the argument String is not escaped, and
-- therefore may contain xml tags or xml entities
unescaped :: String -> XMLBuilder
unescaped = append . textBS

element :: String -> XMLBuilder -> XMLBuilder
element s = either fail builder . buildXML s

tag :: String -> XMLBuilder
tag s = element s (return ())

builder :: Element -> XMLBuilder
builder = append . elementBS

attribute :: Attr -> XMLBuilder
attribute = append . attrBS

(.=.) :: String -> String -> XMLBuilder
n .=. s = attribute (n := escapeAttr s)

escapeAttr :: String -> String
escapeAttr = concatMap f
 where 
   f '<' = "&lt;"
   f '&' = "&amp;"
   f '"' = "&quot;"
   f c   = [c]

fromBuilder :: XMLBuilder -> Maybe Element
fromBuilder m = 
   case runXMLBuilder m of
      Right ([], [Right a]) -> Just a
      _                     -> Nothing

escape :: String -> String
escape = concatMap f
 where
   f '<' = "&lt;"
   f '>' = "&gt;"
   f '&' = "&amp;"
   f c   = [c]

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse