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

module Ideas.Text.MathML
   ( MathML(..), xml2mathml, mathml2xml
   ) where

import Data.Maybe
import Ideas.Text.XML
import Ideas.Utils.Uniplate hiding (children)

data MathML = MRow [MathML]
            | MId String
            | MNumber String
            | MOperator String
            | MString String
            | MText String
            | MSqrt MathML
            | MRoot MathML MathML
            | MSup MathML MathML -- base, superscript
            | MSub MathML MathML -- base, subscript
            | MSubSup MathML MathML MathML -- base, subscript, superscript
            | MFrac MathML MathML -- numerator, denominator
            | MFenced String String MathML -- left, right, content
            | MSpace
            | MStyle
            | MPadded
            | MPhantom
            | MError
            | MEnclose
            | MUnder
            | MOver
            | MUnderOver
            | MTable
            | MTableRow
            | MLabeledTableRow
            | MTableData
   deriving (Show, Eq)
{-
from: https://www.w3.org/TR/MathML2/chapter3.html#presm.mn @ 3.2.1
Conversely, since mn is a presentation element, there are a few situations where it may desirable to include arbitrary text in the content of an mn that should merely render as a numeric literal
-}

instance ToXML MathML where
   toXML = mathml2xml

instance InXML MathML where
   fromXML = either fail return . xml2mathml

instance Uniplate MathML where
   uniplate math =
      case math of
         MRow xs       -> plate MRow ||* xs
         _             -> plate math

----------------------------------------------------------
-- conversion functions: XML <-> MathML

xml2mathml :: XML -> Either String MathML
xml2mathml = rec
 where
   rec xml = case name xml of
       "mrow"       -> MRow <$> mapM rec (children xml)
       "mi"         -> return (MId (getData xml))
       "mn"         -> return (MNumber (getData xml))
       "mo"         -> return (MOperator (getData xml))
       "ms"         -> return (MString (getData xml))
       "mtext"      -> return (MText (getData xml))
       "mroot"      -> case children xml of
                          [c, d] -> MRoot <$> rec c <*> rec d
                          _ -> fail "invalid mroot"
       "msup"       -> case children xml of
                          [c, d] -> MSup <$> rec c <*> rec d
                          _ -> fail "invalid msup"
       "msub"       -> case children xml of
                          [c, d] -> MSub <$> rec c <*> rec d
                          _ -> fail "invalid msub"
       "msubsup"    -> case children xml of
                          [c, d, e] -> MSubSup <$> rec c <*> rec d <*> rec e
                          _ -> fail "invalid msubsup"
       "mfrac"      -> case children xml of
                          [c, d] -> MFrac <$> rec c <*> rec d
                          _ -> fail "invalid mfrac"
       "mfenced"    -> case children xml of
                          [c] -> MFenced (fromMaybe "(" (findAttribute "open" xml)) (fromMaybe ")" (findAttribute "close" xml)) <$> rec c
                          _ -> fail "invalid mfenced"
       "mspace"     -> return MSpace
       "mtable"     -> return MTable
       "mtr"        -> return MTableRow
       "mlabeledtr" -> return MLabeledTableRow
       "munder"     -> return MUnder
       "mover"      -> return MOver
       "munderover" -> return MUnderOver
       -- below are cases that have 1* arguments, when none-one an mrow is implied.
       "math"       -> impliedMRow xml
       "msqrt"      -> MSqrt <$> impliedMRow xml
       "mphantom"   -> return MPhantom
       "mpadded"    -> return MPadded
       "mstyle"     -> return MStyle
       "merror"     -> return MError
       "mtd"        -> return MTableData
       "menclose"   -> return MEnclose
       _ -> fail ("unsupported MathML: " ++ show xml)

   impliedMRow :: XML -> Either String MathML
   impliedMRow xml =
      case children xml of
         [x] -> rec x
         xs  -> MRow <$> mapM rec xs

mathml2xml :: MathML -> XML
mathml2xml = makeXML "math" . rec
 where
   rec :: MathML -> XMLBuilder
   rec math =
      case math of
         MRow ms           -> element "mrow" (map rec ms)
         MId s             -> element "mi" [string s]
         MNumber s         -> element "mn" [string s]
         MOperator s       -> element "mo" [string s]
         MString s         -> element "ms" [string s]
         MText s           -> element "mtext" [string s]
         MSqrt m           -> element "msqrt" [rec m]
         MRoot m1 m2       -> element "mroot" [rec m1, rec m2]
         MSup m1 m2        -> element "msup" [rec m1, rec m2]
         MSub m1 m2        -> element "msub" [rec m1, rec m2]
         MSubSup m1 m2 m3  -> element "msubsup" [rec m1, rec m2, rec m3]
         MFrac m1 m2       -> element "mfrac" [rec m1, rec m2]
         MFenced s1 s2 m   -> element "mfenced" ["open" .=. s1, "close" .=. s2, rec m]
         MSpace            -> element "mspace" []
         MStyle            -> element "mstyle" []
         MPadded           -> element "mpadded" []
         MPhantom          -> element "mphantom" []
         MError            -> element "merror" []
         MEnclose          -> element "menclose" []
         MUnder            -> element "munder" []
         MOver             -> element "mover" []
         MUnderOver        -> element "munderover" []
         MTable            -> element "mtable" []
         MTableRow         -> element "mtr" []
         MLabeledTableRow  -> element "mlabeledtr" []
         MTableData        -> element "mtd" []