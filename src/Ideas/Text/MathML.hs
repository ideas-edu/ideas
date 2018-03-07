-----------------------------------------------------------------------------
-- Copyright 2016, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Ideas.Text.MathML
   ( MathML(..), xml2mathml, mathml2xml
   ) where

import Ideas.Utils.Uniplate hiding (children)
import Ideas.Text.XML

data MathML = MRow [MathML]
            | MText String
   deriving (Show, Eq)

instance InXML MathML where
   toXML   = mathml2xml
   fromXML = either fail return . xml2mathml

instance Uniplate MathML where
   uniplate math =
      case math of
         MRow xs       -> plate MRow ||* xs
         _             -> plate math

----------------------------------------------------------
-- conversion functions: XML <-> MathML

xml2mathml :: XML -> Either String MathML
xml2mathml xmlTop = 
   case xmlTop of
      Element "math" _ [Right e] -> rec e
      _ -> fail "expected <math> for MathML"
 where
   rec xml 
      | name xml == "mrow" = do
           xs <- mapM rec (children xml)
           return (MRow xs)
      | otherwise = 
           case xml of
              Element "mtext" _ [Left s] -> return (MText s)
              _ -> fail ("unsupported MathML: " ++ show xml)
      

mathml2xml :: MathML -> XML
mathml2xml = makeXML "math" . rec
 where
   rec :: MathML -> XMLBuilder
   rec math = 
      case math of 
         MRow xs -> element "mrow" (map rec xs)
         MText s -> element "mtext" [string s]