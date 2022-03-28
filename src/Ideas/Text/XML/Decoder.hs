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
   , Error, errorStr
   , xTag, xString, xAttr, xEnd
     -- re-exports
   , Alternative(..), MonadReader(..), throwError
   ) where

import Ideas.Utils.Decoding
import Ideas.Text.XML.Data
import Ideas.Text.XML.Builder

type DecoderXML = Decoder () (Error XML) XMLBuilder

evalDecoderXML :: DecoderXML a -> XML -> Either (Error XML) a
evalDecoderXML p = evalDecoder p () . builder

xTag :: String -> DecoderXML a -> DecoderXML a
xTag s p = get >>= \xml -> 
   case headIsXML xml of 
      Just (hd, rest) | show (name hd) == s -> do
         put (xmlToBuilder hd)
         a <- p
         xEnd
         put rest
         return a
      _ -> errorStr "xTag"

xString :: DecoderXML String
xString = do
   xml <- get
   case headIsString xml of 
      Just (s, rest) -> put rest >> return s
      Nothing        -> errorStr "xString"

xAttr :: String -> DecoderXML String
xAttr s = get >>= \xml -> 
   case filter (\(n := _) -> show n == s) (builderAttributes xml) of
      [_ := v] -> return v
      _ -> errorStr "xAttr"

xEnd :: DecoderXML ()
xEnd = get >>= \xml -> 
   if contentIsEmpty xml 
   then return () 
   else errorStr "xEnd"