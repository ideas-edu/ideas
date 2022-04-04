{-# LANGUAGE OverloadedStrings #-}
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

module Ideas.Text.OpenMath.Object
   ( OMOBJ(..), getOMVs, xml2omobj, omobj2xml
   ) where

import Data.Char
import Data.Generics.Uniplate.Direct hiding (children)
import Data.List (nub)
import Data.Maybe
import Ideas.Text.OpenMath.Symbol
import Ideas.Text.XML.Decoder
import Ideas.Text.XML
import Ideas.Utils.Decoding

-- internal representation for OpenMath objects
data OMOBJ = OMI Integer
           | OMF Double
           | OMV String
           | OMS Symbol
           | OMA [OMOBJ]
           | OMBIND OMOBJ [String] OMOBJ
   deriving (Show, Eq)

instance ToXML OMOBJ where
   toXML = omobj2xml

instance InXML OMOBJ where
   xmlDecoder = xmlTag "OMOBJ" rec
    where
      rec  =  xmlTag "OMA" (OMA <$> many rec)
          <|> xmlTag "OMS" (makeOMS <$> optional (xmlAttr "cd") <*> xmlAttr "name")
          <|> xmlTag "OMI" (OMI . fromJust . readInt <$> xmlString)
          <|> xmlTag "OMF" (OMF . fromJust . readDouble <$> xmlAttr "dec")
          <|> xmlTag "OMV" (OMV <$> xmlAttr "name")
          <|> xmlTag "OMBIND" (OMBIND <$> rec <*> recOMBVar <*> rec)

      recOMBVar = xmlTag "OMBVAR" (many (xmlTag "OMV" (xmlAttr "name")))

      makeOMS (Just "unknown") a = OMS (Nothing, a)
      makeOMS cd a = OMS (cd, a)
   
instance Uniplate OMOBJ where
   uniplate omobj =
      case omobj of
         OMA xs        -> plate OMA ||* xs
         OMBIND a ss b -> plate OMBIND |* a |- ss |* b
         _             -> plate omobj

getOMVs :: OMOBJ -> [String]
getOMVs omobj = nub [ x | OMV x <- universe omobj ]

----------------------------------------------------------
-- conversion functions: XML <-> OMOBJ

xml2omobj :: XML -> Either String OMOBJ
xml2omobj = either (Left . show) (Right . fst) . runDecoder xmlDecoder () . builder

omobj2xml :: OMOBJ -> XML
omobj2xml object = makeXML "OMOBJ" $ mconcat
   [ "xmlns"   .=. "http://www.openmath.org/OpenMath"
   , "version" .=. "2.0"
   , "cdbase"  .=. "http://www.openmath.org/cd"
   , rec object
   ]
 where
   rec :: OMOBJ -> XMLBuilder
   rec omobj =
      case omobj of
         OMI i  -> element "OMI" [text i]
         OMF f  -> element "OMF" ["dec" .=. show f]
         OMV v  -> element "OMV" ["name" .=. v]
         OMA xs -> element "OMA" (map rec xs)
         OMS s  -> element "OMS"
            [ "cd"   .=. fromMaybe "unknown" (dictionary s)
            , "name" .=. symbolName s
            ]
         OMBIND x ys z -> element "OMBIND"
            [ rec x
            , element "OMBVAR" (map (rec . OMV) ys)
            , rec z
            ]

readInt :: String -> Maybe Integer
readInt s = case reads s of
               [(n, xs)] | all isSpace xs -> Just n
               _ -> Nothing

readDouble :: String -> Maybe Double
readDouble s = case reads s of
                  [(n, xs)] | all isSpace xs -> Just n
                  _ -> Nothing