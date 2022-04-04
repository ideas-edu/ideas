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
-----------------------------------------------------------------------------

module Ideas.Text.XML.Builder 
   ( BuildXML(..)
   , XMLBuilder, makeXML, makeXMLBuilder
   ) where

import Data.String
import Ideas.Text.XML.Data
import Ideas.Text.XML.Document (Name)
import Ideas.Text.XML.Attributes
import Ideas.Utils.Decoding

infix 7 .=.

class (Semigroup a, Monoid a) => BuildXML a where
   (.=.)    :: Name -> String -> a   -- attribute
   string   :: String -> a           -- (escaped) text
   builder  :: XML -> a              -- (named) xml element
   tag      :: Name -> a -> a        -- tag (with content)
   -- functions with a default
   char     :: Char -> a
   text     :: Show s => s -> a      -- escaped text with Show class
   element  :: Name -> [a] -> a
   emptyTag :: Name -> a
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

-------------------------------------------------------------------

data XMLBuilder = B Attributes Content

instance Semigroup XMLBuilder where
  B as1 elts1 <> B as2 elts2 = B (as1 <> as2) (elts1 <> elts2)

instance Monoid XMLBuilder where
   mempty  = B mempty mempty
   mappend = (<>)

instance BuildXML XMLBuilder where
   n .=. s  = B (attribute n s) mempty
   string s = B mempty (if null s then mempty else fromString s)
   builder  = B mempty . xmlToContent
   tag n    = builder . makeXML n

instance IsString XMLBuilder where
   fromString = string

instance HasContent XMLBuilder where
   updateContent (B as c) = (c, B as)

instance HasAttributes XMLBuilder where
   updateAttributes (B as c) = (as, \bs -> B bs c)

makeXMLBuilder :: Attributes -> Content -> XMLBuilder
makeXMLBuilder = B

makeXML :: Name -> XMLBuilder -> XML
makeXML n (B as c) = xmlRoot n as c