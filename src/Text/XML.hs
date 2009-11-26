-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
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
module Text.XML
   ( XML, Attr, AttrList, InXML(..), Element(..)
   , XMLBuilder, makeXML, text, unescaped, element, tag, attribute
   , parseXML, showXML, compactXML, (.=.), findAttribute
   , children, Attribute(..), builder, findChild, getData {-, extract, extractText -}
   , {- isText, isTag, mkTag mkText , findChild-}
   ) where

import Common.Utils (trim)
import Control.Monad.State
import Control.Monad.Error ()
import Data.Char
import Data.List
import Data.Monoid
import Text.XML.Interface hiding (parseXML)
import qualified Text.XML.Interface as I

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
      | name xml == "list" && noAttributes xml = mapM fromXML (children xml)
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
          cd n = Left ('\n' : replicate n ' ')
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

-- Uses the fast-append trick on lists
data BuilderState = BS { bsAttributes :: AttrList -> AttrList, bsElements :: Content -> Content }

-- local helper
emptyBS :: BuilderState
emptyBS = BS id id

appendAttrBS :: Attr -> BuilderState -> BuilderState
appendAttrBS a bs = bs { bsAttributes = bsAttributes bs . (a:) }

appendElemBS :: Either String Element -> BuilderState -> BuilderState
appendElemBS e bs = bs { bsElements = bsElements bs . (e:) }

type XMLBuilder = XMLBuilderM ()

newtype XMLBuilderM a = XMLBuilder { unBuild :: State BuilderState a }

instance Monoid a => Monoid (XMLBuilderM a) where
   mempty  = return mempty
   mappend = (>>)

instance Monad XMLBuilderM where
   return  = XMLBuilder . return
   m >>= f = XMLBuilder (unBuild m >>= (unBuild . f))

makeXML :: String -> XMLBuilder -> XML
makeXML s m = 
   let bs = execState (unBuild m) emptyBS
   in Element s (bsAttributes bs []) (bsElements bs [])

text :: String -> XMLBuilder
text = unescaped . escape

-- Should be used with care: the argument String is not escaped, and
-- therefore may contain xml tags or xml entities
unescaped :: String -> XMLBuilder
unescaped = XMLBuilder . modify . appendElemBS . Left

element :: String -> XMLBuilder -> XMLBuilder
element s = XMLBuilder . modify . appendElemBS . Right . makeXML s

tag :: String -> XMLBuilder
tag s = element s (return ())

attribute :: Attr -> XMLBuilder
attribute = XMLBuilder . modify . appendAttrBS

(.=.) :: String -> String -> XMLBuilder
n .=. s = attribute (n := s)

builder :: Element -> XMLBuilder
builder = XMLBuilder . modify . appendElemBS . Right

escape :: String -> String
escape = concatMap f
 where
   f '<' = "&lt;"
   f '>' = "&gt;"
   f '&' = "&amp;"
   f c   = [c]

----------------------------------------------------------------
-- XML utility functions

{-
children :: XML -> [XML]
children (Element _ _ xs) = xs

extract :: Monad m => String -> XML -> m [XML]
extract n xml =
   case filter (children xml) of --  [ xs | Tagged (Element m _ xs) <- children xml, n==m ] of
      [hd] -> return hd
      _    -> fail ("missing tag " ++ show n)

extractText :: Monad m => String -> XML -> m String
extractText n xml = do
   xs <- extract n xml
   case xs of
      [hd] -> maybe (fail "extract text") return (isText hd)
      _    -> fail ("invalid content for tag " ++ show n)

isTag :: XML -> Maybe (String, AttrList, [XML])
isTag = 
isTag (Tagged (Element n as xs)) = 
   let f (x := y) = (x, concatMap (either return g) y) 
       g (CharRef n) = [chr n]
       g (EntityRef n)
          | otherwise = []
   in Just (n, map f as, xs)
isTag _ = Nothing 

mkTag :: String -> AttrList -> Content -> XML
mkTag n as = Element n (map f as)
 where
   f (x, y) = x := y

mkText :: String -> XML
mkText = -- CharData

isText :: XML -> Maybe String
isText =
isText (CharData s) = Just s
isText (CDATA s)    = Just s
isText _            = Nothing 

findChild :: Monad m => String -> XML -> m XML
findChild s e = maybe (fail "child not found") return . safeHead $ 
   [ c | c <- children e, Just (n, _, _) <- [isTag c], s==n ]-}