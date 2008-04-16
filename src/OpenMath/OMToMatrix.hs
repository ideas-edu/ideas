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
-- (...add description...)
--
-----------------------------------------------------------------------------
module OpenMath.OMToMatrix where

import Text.XML.HaXml.Xml2Haskell
import OpenMath.OpenMath
import Data.Maybe
import Control.Monad
import Domain.LinearAlgebra

xml2matrix :: Read a => String -> Maybe (Matrix a)
xml2matrix input = readXml input >>= omobj2matrix

matrix2xml :: Show a => Matrix a -> String
matrix2xml = showXml . matrix2omobj

{----------------------------------------------------------------
From OMOBJ, the data type for OpenMath documents in Haskell
to a Matrix data type.
----------------------------------------------------------------}

omobj2matrix :: Read a => OMOBJ -> Maybe (Matrix a)
omobj2matrix omobj = 
  case omobj of
    OMOBJOMA _ (OMA _ (NonEmpty (_:oma_s))) -> oma_s2matrix oma_s
    _ -> Nothing
    
oma_s2matrix :: Read a => [OMA_] -> Maybe (Matrix a)
oma_s2matrix oma_s = do
   xs <- mapM oma_2row oma_s
   guard (isRectangular xs)
   return (makeMatrix xs)

oma_2row :: Read a => OMA_ -> Maybe [a]
oma_2row oma_ = 
  case oma_ of
    OMA_OMA (OMA _ (NonEmpty (_:oma_omis))) -> mapM oma_omi2a oma_omis
    _ -> Nothing

oma_omi2a :: Read a => OMA_ -> Maybe a     
oma_omi2a oma_ =
  case oma_ of
    OMA_OMI (OMI _ s) -> case reads s of [(a, _)] -> Just a; _ -> Nothing
    _ -> Nothing

{----------------------------------------------------------------
From a Matrix data type
to OMOBJ, the data type for OpenMath documents in Haskell.
----------------------------------------------------------------}

matrix2omobj :: Show a => Matrix a -> OMOBJ
matrix2omobj matrix = 
  OMOBJOMA
    (OMOBJ_Attrs {oMOBJXmlns = NonDefault "http://www.openmath.org/OpenMath"
                 ,oMOBJId = Nothing
                 ,oMOBJCdbase = Just "http://www.openmath.org/cd"
                 , oMOBJVersion = Just "2.0"}) 
    (OMA (OMA_Attrs {oMAXmlns = Default "http://www.openmath.org/OpenMath"
                    ,oMAId = Nothing
                    ,oMACdbase = Nothing}) 
         (NonEmpty (OMA_OMS (OMS {oMSXmlns = Default "http://www.openmath.org/OpenMath"
                                 ,oMSId = Nothing
                                 ,oMSName = "matrix"
                                 ,oMSCd = "linalg2"
                                 ,oMSCdbase = Nothing}):
                   map row2oma_ (rows matrix))))

row2oma_ :: Show a => [a] -> OMA_
row2oma_ row = 
  OMA_OMA (OMA (OMA_Attrs {oMAXmlns = Default "http://www.openmath.org/OpenMath"
                          ,oMAId = Nothing
                          ,oMACdbase = Nothing}) 
               (NonEmpty ((OMA_OMS (OMS {oMSXmlns = Default "http://www.openmath.org/OpenMath"
                                       ,oMSId = Nothing
                                       ,oMSName = "matrixrow"
                                       ,oMSCd = "linalg2"
                                       ,oMSCdbase = Nothing})):
                          map elt2string row)))

-- the spaces around the element are needed to get literal equality 
elt2string :: Show a => a -> OMA_
elt2string a = OMA_OMI (OMI (OMI_Attrs {oMIXmlns = Default "http://www.openmath.org/OpenMath"
                                       ,oMIId = Nothing}) 
                            (" " ++ show a ++ " "))

{----------------------------------------------------------------
A simple test.
----------------------------------------------------------------}

test2 = do 
   input <- readFile "src/Openmath/twoxtwomatrix.xml"
   let m1  = fromJust $ xml2matrix input :: Matrix Int
       xml = matrix2xml m1
       m2  = fromJust $ xml2matrix xml :: Matrix Int
   print m1
   putStrLn xml
   print m2
   print (m1==m2)