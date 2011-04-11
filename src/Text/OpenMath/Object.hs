{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
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
   ( OMOBJ(..), getOMVs, xml2omobj, omobj2xml
   ) where

import Control.Monad
import Data.Generics.Uniplate hiding (children)
import Data.List (nub)
import Data.Maybe
import Data.Typeable
import Text.OpenMath.Symbol
import Text.XML
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.Parsing

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
         OMA xs        -> (xs, OMA)
         OMBIND a ss b -> ([a, b], \[x, y] -> OMBIND x ss y)
         _             -> ([], \_ -> omobj)

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
            case scanInt s of
               Just i -> return (OMI (toInteger i))
               _      -> fail "invalid integer in OMI"
         
         [] | name xml == "OMF" -> do
            s <- findAttribute "dec" xml 
            case scanNumber s of
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
omobj2xml object = makeXML "OMOBJ" $ do
   "xmlns"   .=. "http://www.openmath.org/OpenMath"
   "version" .=. "2.0"
   "cdbase"  .=. "http://www.openmath.org/cd"
   rec object
 where
   rec omobj =
      case omobj of
         OMI i  -> element "OMI" (text (show i))
         OMF f  -> element "OMF" ("dec" .=. show f)
         OMV v  -> element "OMV" ("name" .=. v)
         OMA xs -> element "OMA" (mapM_ rec xs)
         OMS s  -> element "OMS" $ do
            "cd"   .=. fromMaybe "unknown" (dictionary s)
            "name" .=. symbolName s
         OMBIND x ys z -> element "OMBIND" $ do 
            rec x 
            element "OMBVAR" (mapM_ (rec . OMV) ys) 
            rec z

scanInt :: String -> Maybe Integer     
scanInt = either (const Nothing) Just . parseSimple (P.integer lexer)

scanNumber :: String -> Maybe Double   
scanNumber = either (error . show) Just . parseSimple p
 where
   p = option id (P.reservedOp lexer "-" >> return negate) <*> P.float lexer

lexer :: P.TokenParser a
lexer = P.makeTokenParser $ emptyDef {reservedOpNames = ["-"]}