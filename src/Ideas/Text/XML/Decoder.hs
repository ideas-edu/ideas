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
xTag s p = do
   xml <- get
   case fromBS xml of 
      (as, Right hd:rest) | name hd == s -> do
         put (conv (attributes hd) (content hd))
         a <- p
         xEnd
         put (conv as rest)
         return a
      _ -> errorStr "xTag"

xString :: DecoderXML String
xString = do
   xml <- get
   case fromBS xml of 
      (as, Left s:rest) -> do
         put (conv as rest)
         return s
      _ -> errorStr "xString"

xAttr :: String -> DecoderXML String
xAttr s = do
   xml <- get
   let (as, _) = fromBS xml
   case filter (\(n := _) -> n == s) as of
      [_ := v] -> return v
      _ -> errorStr "xAttr"

xEnd :: DecoderXML ()
xEnd = do
   xml <- get
   case fromBS xml of
      (_, []) -> do
         return ()
      _ -> errorStr "xEnd"

conv :: Attributes -> [Either String XML] -> XMLBuilder
conv as xs = mconcat (map f as ++ map g xs)
 where
   f (n := s)  = n .=. s
   g (Left s)  = string s
   g (Right a) = builder a  