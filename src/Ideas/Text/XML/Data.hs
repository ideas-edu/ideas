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

-- to do: protect constructor Tag and invariant

module Ideas.Text.XML.Data 
   ( -- types
     XML(..), Attributes, Attribute(..)
   , D.Name, validName
     -- pretty-printing
   , prettyXML, compactXML
     -- processing
   , foldXML, trimXML
   ) where

import Data.Char (isSpace, ord)
import qualified Ideas.Text.XML.Document as D
import Ideas.Text.XML.Unicode

-------------------------------------------------------------------------------
-- XML types

-- invariants content: no two adjacent Lefts, no Left with empty string,
-- valid tag/attribute names
data XML = Tag
   { name       :: D.Name
   , attributes :: Attributes
   , content    :: [Either String XML]
   }
 deriving Eq

instance Show XML where
   show = compactXML

type Attributes = [Attribute]

data Attribute = D.Name := String
 deriving Eq

validName :: String -> Bool
validName []     = False
validName (x:xs) = (isLetter x || x `elem` "_:") && all validNameChar xs

validNameChar :: Char -> Bool
validNameChar c = any ($ c) [isLetter, isDigit, isCombiningChar, isExtender, (`elem` ".-_:")]

-------------------------------------------------------------------------------
-- Pretty-printing XML

prettyXML :: XML -> String
prettyXML = show . D.prettyElement False . toElement

compactXML :: XML -> String
compactXML = show . D.prettyElement True . toElement

toElement :: XML -> D.Element
toElement = foldXML make mkAttribute mkString
 where
   make n as = D.Element n as . concatMap (either id (return . D.Tagged))

   mkAttribute :: Attribute -> D.Attribute
   mkAttribute (m := s) = (D.:=) m (map Left s)

   mkString :: String -> [D.XML]
   mkString [] = []
   mkString xs@(hd:tl)
      | null xs1  = D.Reference (D.CharRef (ord hd)) : mkString tl
      | otherwise = D.CharData xs1 : mkString xs2
    where
      (xs1, xs2) = break ((> 127) . ord) xs

-------------------------------------------------------------------------------
-- Processing XML

foldXML :: (D.Name -> [a] -> [Either s e] -> e) -> (Attribute -> a) -> (String -> s) -> XML -> e
foldXML fe fa fs = rec
 where
   rec (Tag n as cs) = fe n (map fa as) (map (either (Left . fs) (Right . rec)) cs)

trimXML :: XML -> XML
trimXML = foldXML Tag f trim
 where
   f (n := s) = n := trim s

trim, trimLeft, trimRight :: String -> String
trim      = trimLeft . trimRight
trimLeft  = dropWhile isSpace
trimRight = reverse . trimLeft . reverse