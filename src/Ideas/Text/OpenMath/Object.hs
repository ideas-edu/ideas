{-# LANGUAGE DeriveDataTypeable #-}
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
-----------------------------------------------------------------------------

module Ideas.Text.OpenMath.Object
   ( OMOBJ(..), getOMVs, xml2omobj, omobj2xml
   ) where

import Data.Char
import Data.Generics.Uniplate.Direct hiding (children)
import Data.List (nub)
import Data.Maybe
import Data.Typeable
import Ideas.Text.OpenMath.Symbol
import Ideas.Text.XML

-- internal representation for OpenMath objects
data OMOBJ = OMI Integer
           | OMF Double
           | OMV String
           | OMS Symbol
           | OMA [OMOBJ]
           | OMBIND OMOBJ [String] OMOBJ
   deriving (Show, Eq, Typeable)

instance InXML OMOBJ where
   toXML   = omobj2xml
   fromXML = either fail return . xml2omobj

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
xml2omobj xmlTop =
   case xmlTop of
      Element "OMOBJ" _ [Right e] -> rec e
      _ -> fail $ "expected an OMOBJ tag" ++ show xmlTop
 where
   rec xml =
      case content xml of

         _ | name xml == "OMA" -> do
            ys <- mapM rec (children xml)
            return (OMA ys)

         [] | name xml == "OMS" -> do
            let mcd = findAttribute "cd" xml
            n <- findAttribute "name" xml
            return (OMS (mcd, n))

         [Left s] | name xml == "OMI" ->
            case readInt s of
               Just i -> return (OMI (toInteger i))
               _      -> fail "invalid integer in OMI"

         [] | name xml == "OMF" -> do
            s <- findAttribute "dec" xml
            case readDouble s of
               Just nr -> return (OMF nr)
               _       -> fail "invalid floating-point in OMF"

         [] | name xml == "OMV" -> do
            s <- findAttribute "name" xml
            return (OMV s)

         [Right x1, Right x2, Right x3] | name xml == "OMBIND" -> do
            y1 <- rec x1
            y2 <- recOMBVAR x2
            y3 <- rec x3
            return (OMBIND y1 y2 y3)

         _ -> fail ("invalid tag " ++ show (name xml))

   recOMBVAR xml
      | name xml == "OMBVAR" =
           let f (Right (OMV s)) = return s
               f this = fail $ "expected tag OMV in OMBVAR, but found " ++ show this
           in mapM (f . rec) (children xml)
      | otherwise =
           fail ("expected tag OMVAR, but found " ++ show (name xml))

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