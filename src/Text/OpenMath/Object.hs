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
module Text.OpenMath.Object 
   ( OMOBJ(..), xml2omobj, omobj2xml
   ) where

import Text.XML
import Data.Char (isSpace)

-- internal representation for OM objects (close to XML)
data OMOBJ = OMI Integer 
           | OMF Float 
           | OMV String 
           | OMS String String 
           | OMA [OMOBJ] 
           |OMBIND OMOBJ [String] OMOBJ
   deriving (Show, Eq)

instance InXML OMOBJ where
   toXML   = omobj2xml
   fromXML = either fail return . xml2omobj

----------------------------------------------------------
-- conversion functions: XML <-> OMOBJ
   
xml2omobj :: XML -> Either String OMOBJ
xml2omobj xml =
   case xml of  
      Element "OMOBJ" _ [Right e] -> rec e
      _ -> fail $ "expected an OMOBJ tag" ++ show xml
 where
   rec xml =
      case content xml of
      
         _ | name xml == "OMA" -> do
            ys <- mapM rec (children xml) 
            return (OMA ys)
            
         [] | name xml == "OMS" -> do
            cd   <- findAttribute "cd" xml
            name <- findAttribute "name" xml
            return (OMS cd name)

         [Left s] | name xml == "OMI" ->
            case reads s of
               [(i, xs)] | all isSpace xs -> return (OMI i)
               _ -> fail "invalid integer in OMI"
         
         [] | name xml == "OMF" -> do
            s <- findAttribute "dec" xml 
            case reads s of
               [(fp, xs)] | all isSpace xs -> return (OMF fp)
               _ -> fail "invalid floating-point in OMF"
                    
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
           fail ("expected tag OMVAR, but found " ++ show tag)
   
omobj2xml :: OMOBJ -> XML
omobj2xml object = makeXML "OMOBJ" $ do
   "xmlns"   .=. "http://www.openmath.org/OpenMath"
   "version" .=. "2.0"
   "cdbase"  .=. "http://www.openmath.org/cd"
   rec object
 where
   rec omobj =
      case omobj of
         OMI i       -> element "OMI" (text (show i))
         OMF f       -> element "OMF" ("dec" .=. show f)
         OMV v       -> element "OMV" ("name" .=. v)
         OMA xs      -> element "OMA" (mapM_ rec xs)
         OMS cd name -> element "OMS" $ do
            "cd"   .=. cd
            "name" .=. name
         OMBIND x ys z -> element "OMBIND" $ do 
            rec x 
            element "OMBVAR" (mapM_ (rec . OMV) ys) 
            rec z