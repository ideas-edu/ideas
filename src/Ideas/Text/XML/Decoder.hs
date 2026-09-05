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

module Ideas.Text.XML.Decoder
   ( DecoderXML, evalDecoderXML
   , xmlTag, xmlString, xmlAttr, xmlEnd
   ) where

import Data.String
import Ideas.Utils.Decoding
import Ideas.Text.XML.Attributes
import Ideas.Text.XML.Data
import Ideas.Text.XML.Builder
import Ideas.Text.XML.Document (Name)

type DecoderXML = Decoder () (Error XML) XMLBuilder

evalDecoderXML :: DecoderXML a -> XML -> Either (Error XML) a
evalDecoderXML p = evalDecoder p () . builder

xmlTag :: Name -> DecoderXML a -> DecoderXML a
xmlTag n p = get >>= \xml -> 
   case headIsXML xml of 
      Just (hd, rest) | getName hd == n -> do
         put (makeXMLBuilder (getAttributes hd) (getContent hd))
         a <- p
         xmlEnd
         put rest
         return a
      _ -> errorStr "xTag"

xmlString :: DecoderXML String
xmlString = do
   xml <- get
   case headIsString xml of 
      Just (s, rest) -> put rest >> return s
      Nothing        -> errorStr "xString"

xmlAttr :: String -> DecoderXML String
xmlAttr s = get >>= \xml -> 
   case lookupAttribute (fromString s) (getAttributes xml) of
      Just v -> return v
      _ -> errorStr "xAttr"

xmlEnd :: DecoderXML ()
xmlEnd = get >>= \xml -> 
   if contentIsEmpty xml 
   then return () 
   else errorStr "xEnd"