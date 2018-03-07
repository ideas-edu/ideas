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

import Data.Either
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
xml2mathml xmlTop = rec xmlTop
 where
   rec xml = case xml of
       Element "mrow" _ _ -> MRow <$> mapM rec (children xml)
       Element "mi" _ [Left s] -> return (MId s)
       Element "mn" _ [Left s] -> return (MNumber s)
       Element "mo" _ [Left s] -> return (MOperator s)
       Element "ms" _ [Left s] -> return (MString s)
       Element "mtext" _ [Left s] -> return (MText s)
       Element "mroot" _ [Right c, Right d] -> MRoot <$> rec c <*> rec d
       Element "msup" _ [Right c, Right d] -> MSup <$> rec c <*> rec d
       Element "msub" _ [Right c, Right d] -> MSub <$> rec c <*> rec d
       Element "msubsup" _ [Right c, Right d, Right e] -> MSubSup <$> rec c <*> rec d <*> rec e
       Element "mfrac" _ [Right c, Right d] -> MFrac <$> rec c <*> rec d
       Element "mfenced" _ [Right c] -> MFenced (fromMaybe "(" (findAttribute "open" xml)) (fromMaybe ")" (findAttribute "close" xml)) <$> rec c
       Element "mspace" _ _ -> return MSpace
       Element "mtable" _ _ -> return MTable
       Element "mtr" _ _ -> return MTableRow
       Element "mlabeledtr" _ _ -> return MLabeledTableRow
       Element "munder" _ _ -> return MUnder
       Element "mover" _ _ -> return MOver
       Element "munderover" _ _ -> return MUnderOver
       -- below are cases that have 1* arguments, when none-one an mrow is implied.
       Element "math" _ xs -> impliedMRow xs
       Element "msqrt" _ xs -> MSqrt <$> impliedMRow xs
       Element "mphantom" _ _ -> return MPhantom
       Element "mpadded" _ _ -> return MPadded
       Element "mstyle" _ _ -> return MStyle
       Element "merror" _ _ -> return MError
       Element "mtd" _ _ -> return MTableData
       Element "menclose" _ _ -> return MEnclose
       _ -> fail ("unsupported MathML: " ++ show xml)
   impliedMRow :: [Either String Element] -> Either String MathML
   impliedMRow [Right r] = rec r
   impliedMRow xs = MRow <$> mapM rec (rights xs)


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










fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "fromRight"

example :: XML
example = fromRight $ parseXML "<math><mrow><ms>Hi</ms><mo>+</mo><mi>x</mi><mroot><mn>2</mn><mn>4</mn></mroot><mfenced open=\"{\" close=\"}\"><mrow><mn>3</mn><mo>*</mo><mi>z</mi></mrow></mfenced></mrow></math>"
