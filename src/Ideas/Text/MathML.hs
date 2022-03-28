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

import Ideas.Text.XML
import Ideas.Text.XML.Decoder
import Ideas.Utils.Uniplate hiding (children)
import Ideas.Utils.Decoding

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
   xmlDecoder = rec 
    where
      rec  =  xTag "mrow" (MRow <$> many rec)
          <|> xTag "msqrt" (MSqrt <$> impliedRow)
          <|> xTag "math" impliedRow
          <|> xTag "mi" (MId <$> xString)
          <|> xTag "mn" (MNumber <$> xString)
          <|> xTag "mo" (MOperator <$> xString)
          <|> xTag "ms" (MString <$> xString)
          <|> xTag "mtext" (MText <$> xString)
          <|> xTag "mroot" (MRoot <$> rec <*> rec)
          <|> xTag "msup" (MSup <$> rec <*> rec)
          <|> xTag "msub" (MSub <$> rec <*> rec)
          <|> xTag "msubsup" (MSubSup <$> rec <*> rec <*> rec)
          <|> xTag "mfrac" (MFrac <$> rec <*> rec)
          <|> xTag "mfenced" (MFenced <$> xAttr "open" <*> xAttr "close" <*> rec)
          <|> xTag "mspace" (return MSpace)
          <|> xTag "mstyle" (return MStyle)
          <|> xTag "mpadded" (return MPadded)
          <|> xTag "mphantom" (return MPhantom)
          <|> xTag "merror" (return MError)
          <|> xTag "menclose" (return MEnclose)
          <|> xTag "munder" (return MUnder)
          <|> xTag "mover" (return MOver)
          <|> xTag "munderover" (return MUnderOver)
          <|> xTag "mtable" (return MTable)
          <|> xTag "mtr" (return MTableRow)
          <|> xTag "mlabeledtr" (return MLabeledTableRow)
          <|> xTag "mtd" (return MTableData)

      impliedRow = f <$> many rec

      f [x] = x
      f xs  = MRow xs

instance Uniplate MathML where
   uniplate math =
      case math of
         MRow xs -> plate MRow ||* xs
         _       -> plate math

----------------------------------------------------------
-- conversion functions: XML <-> MathML

xml2mathml :: XML -> Either String MathML
xml2mathml = either (Left . show) (Right . fst) . runDecoder xmlDecoder () . builder

mathml2xml :: MathML -> XML
mathml2xml = makeXML (fromString "math") . rec
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