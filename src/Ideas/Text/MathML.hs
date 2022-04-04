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

module Ideas.Text.MathML
   ( MathML(..), xml2mathml, mathml2xml
   ) where

import Ideas.Text.XML
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
      rec  =  xmlTag "mrow" (MRow <$> many rec)
          <|> xmlTag "msqrt" (MSqrt <$> impliedRow)
          <|> xmlTag "math" impliedRow
          <|> xmlTag "mi" (MId <$> xmlString)
          <|> xmlTag "mn" (MNumber <$> xmlString)
          <|> xmlTag "mo" (MOperator <$> xmlString)
          <|> xmlTag "ms" (MString <$> xmlString)
          <|> xmlTag "mtext" (MText <$> xmlString)
          <|> xmlTag "mroot" (MRoot <$> rec <*> rec)
          <|> xmlTag "msup" (MSup <$> rec <*> rec)
          <|> xmlTag "msub" (MSub <$> rec <*> rec)
          <|> xmlTag "msubsup" (MSubSup <$> rec <*> rec <*> rec)
          <|> xmlTag "mfrac" (MFrac <$> rec <*> rec)
          <|> xmlTag "mfenced" (MFenced <$> xmlAttr "open" <*> xmlAttr "close" <*> rec)
          <|> xmlTag "mspace" (return MSpace)
          <|> xmlTag "mstyle" (return MStyle)
          <|> xmlTag "mpadded" (return MPadded)
          <|> xmlTag "mphantom" (return MPhantom)
          <|> xmlTag "merror" (return MError)
          <|> xmlTag "menclose" (return MEnclose)
          <|> xmlTag "munder" (return MUnder)
          <|> xmlTag "mover" (return MOver)
          <|> xmlTag "munderover" (return MUnderOver)
          <|> xmlTag "mtable" (return MTable)
          <|> xmlTag "mtr" (return MTableRow)
          <|> xmlTag "mlabeledtr" (return MLabeledTableRow)
          <|> xmlTag "mtd" (return MTableData)

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