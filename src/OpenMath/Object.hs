-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module OpenMath.Object 
   ( OMOBJ(..), xml2omobj, omobj2xml
   ) where

import Service.XML
import Data.Char (isSpace)

-- internal representation for OM objects (close to XML)
data OMOBJ = OMI Integer | OMV String | OMS String String | OMA [OMOBJ] |OMBIND OMOBJ [String] OMOBJ
   deriving (Show, Eq)

----------------------------------------------------------
-- conversion functions: XML <-> OMOBJ
   
xml2omobj :: XML -> Either String OMOBJ
xml2omobj xml =
   case xml of  
      Tag "OMOBJ" _ [this] -> rec this
      _                    -> fail "expected a OMOBJ tag"
 where
   rec xml =
      case xml of
         Tag "OMA" _ xs -> do
            xs <- mapM rec xs 
            return (OMA xs)
         Tag "OMS" attrs [] -> 
            case lookup "cd" attrs of
               Just cd ->
                  case lookup "name" attrs of
                     Just name  -> return (OMS cd name)
                     Nothing -> fail "OMS tag without name attribute" 
               _ -> fail "OMS tag without cd attribute"
         Tag "OMI" _ [Text s] -> 
            case reads s of
               [(i, xs)] | all isSpace xs -> return (OMI i)
               _ -> fail "invalid integer in OMI"
         Tag "OMV" attrs [] ->
            case lookup "name" attrs of
               Just s  -> return (OMV s)
               Nothing -> fail "OMV tag without name attribute"
         Tag "OMBIND" _ [x1,x2,x3] -> do
            y1 <- rec x1
            y2 <- recOMBVAR x2
            y3 <- rec x3
            return (OMBIND y1 y2 y3)
            
         Tag tag _ _ -> fail $ "unknown tag " ++ show tag
         Text _ -> fail "expecting a tag"
   
   recOMBVAR xml = 
      case xml of
         Tag "OMBVAR" _ xs -> 
            let f (Right (OMV s)) = return s
                f this = fail $ "expected tag OMV in OMBVAR, but found " ++ show this
            in mapM (f . rec) xs
         Tag tag _ _  -> fail $ "expected tag OMVAR, but found " ++ show tag
         Text _ -> fail "expecting a tag"
   
omobj2xml :: OMOBJ -> XML
omobj2xml = header . return . rec
 where
   header = Tag "OMOBJ" attrs
   attrs  = [ ("xmlns"  , "http://www.openmath.org/OpenMath")
            , ("version", "2.0")
            , ("cdbase" , "http://www.openmath.org/cd")
            ]
   rec omobj =
      case omobj of
         OMI i  -> Tag "OMI" [] [Text (show i)]
         OMV v  -> Tag "OMV" [("name", v)] []
         OMA xs -> Tag "OMA" [] (map rec xs)
         OMS cd name -> Tag "OMS" [("cd", cd), ("name", name)] []
         OMBIND x ys z -> Tag "OMBIND" [] [rec x, Tag "OMBVAR" [] (map (rec . OMV) ys), rec z]