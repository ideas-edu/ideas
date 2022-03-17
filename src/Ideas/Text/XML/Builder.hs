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

-- to do: hide local function fromBS

module Ideas.Text.XML.Builder 
   ( BuildXML(..)
   , XMLBuilder, makeXML, fromBS
   ) where

import Data.List (nubBy)
import Data.Foldable (toList)
import Data.String
import qualified Data.Sequence as Seq
import qualified Data.Map as M
import Ideas.Text.XML.Data
import Ideas.Utils.Decoding

infix 7 .=.

class (Semigroup a, Monoid a) => BuildXML a where
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

-------------------------------------------------------------------

data XMLBuilder = BS (Seq.Seq Attribute) (Seq.Seq (Either String XML))

instance Semigroup XMLBuilder where
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
nameCheck s = if validName s then id else error $ "Invalid name for xml tag: " ++ s

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