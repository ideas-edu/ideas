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
     XML(..)
   , Attributes, Attribute(..)
   , Name, uncheckedName
   , Content, toContent, fromContent, xmlToContent
   , HasContent(..), contentIsEmpty, headIsString, headIsXML
     -- pretty-printing
   , prettyXML, compactXML
     -- processing
   , foldXML, trimXML
   ) where

import Data.Char (isSpace, ord)
import Data.String
import Ideas.Text.XML.Document (Name, uncheckedName)
import qualified Ideas.Text.XML.Document as D

-------------------------------------------------------------------------------
-- XML types

-- invariants content: no two adjacent Lefts, no Left with empty string,
-- valid tag/attribute names
data XML = Tag
   { name       :: Name
   , attributes :: Attributes
   , content    :: Content
   }
 deriving Eq

instance Show XML where
   show = compactXML

type Attributes = [Attribute]

data Attribute = Name := String
 deriving Eq

data Content = Empty
             | CData String
             | Cons XML Content 
             | Mixed String XML Content
 deriving Eq

instance Show Content where
   showsPrec _ = rec
    where
      rec Empty         = id
      rec (CData s)     = (s ++)
      rec (Cons x c)    = (show x ++) . rec c 
      rec (Mixed s x c) = (s ++) . (show x ++) . rec c 

instance Semigroup Content where
   Empty       <> rest        = rest
   CData s     <> Empty       = CData s
   CData s     <> CData t     = CData (s ++ t)
   CData s     <> Cons x c    = Mixed s x c
   CData s     <> Mixed t x c = Mixed (s ++ t) x c  
   Cons x c    <> rest        = Cons x (c <> rest)
   Mixed s x c <> rest        = Mixed s x (c <> rest)   

instance Monoid Content where
   mempty = Empty

instance IsString Content where
   fromString s = if null s then Empty else CData s

class HasContent a where
   getContent    :: a -> Content
   setContent    :: Content -> a -> a
   changeContent :: (Content -> Content) -> a -> a
   updateContent :: a -> (Content, Content -> a)
   
   {-# MINIMAL (getContent, changeContent) | updateContent #-}

   -- default definitions
   getContent      = fst . updateContent
   setContent      = changeContent . const
   changeContent f = (\(c, g) -> g (f c)) . updateContent
   updateContent a = (getContent a, (`setContent` a))

instance HasContent Content where
   updateContent a = (a, id)

instance HasContent XML where
   updateContent xml = (content xml, \new -> xml {content = new})

contentIsEmpty :: HasContent a => a -> Bool
contentIsEmpty a =
   case getContent a of
      Empty -> True
      _     -> False

headIsString :: HasContent a => a -> Maybe (String, a)
headIsString a =
   case updateContent a of
      (CData s, f)     -> Just (s, f Empty)
      (Mixed s x c, f) -> Just (s, f (Cons x c))
      _                -> Nothing

headIsXML :: HasContent a => a -> Maybe (XML, a)
headIsXML a =
   case updateContent a of
      (Cons x c, f) -> Just (x, f c)
      _             -> Nothing

xmlToContent :: XML -> Content
xmlToContent = (`Cons` Empty)

toContent :: [Either String XML] -> Content
toContent []                    = Empty
toContent (Right x:rest)        = Cons x (toContent rest)
toContent (Left s:Left t:rest)  = toContent (Left (s ++ t) : rest)
toContent (Left []:rest)        = toContent rest
toContent (Left s:[])           = CData s
toContent (Left s:Right x:rest) = Mixed s x (toContent rest) 

fromContent :: Content -> [Either String XML]
fromContent Empty         = []
fromContent (CData s)     = [Left s]
fromContent (Cons x c)    = Right x : fromContent c 
fromContent (Mixed s x c) = Left s : Right x : fromContent c

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

foldXML :: (Name -> [a] -> [Either s e] -> e) -> (Attribute -> a) -> (String -> s) -> XML -> e
foldXML fe fa fs = rec
 where
   rec (Tag n as cs) = fe n (map fa as) (map (either (Left . fs) (Right . rec)) (fromContent cs))

trimXML :: XML -> XML
trimXML = foldXML (\n as -> Tag n as . toContent) f trim
 where
   f (n := s) = n := trim s

trim, trimLeft, trimRight :: String -> String
trim      = trimLeft . trimRight
trimLeft  = dropWhile isSpace
trimRight = reverse . trimLeft . reverse