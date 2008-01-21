-- A datatype, parser, and pretty printer for XML documents
module OpenMath.XML 
   ( XML(..), Attr, AttrList
   , parseXML, parseXMLs, showXML
   , children, extract
   ) where

import Common.Utils (trim)
import Control.Monad.Error
import Data.Char
import Data.Maybe
import Data.List

----------------------------------------------------------------
-- Datatype definitions

-- two helper types for attributes
type Attr     = (String, String)
type AttrList = [Attr]
   
data XML = Tag String AttrList [XML] | Text String
   deriving Show

----------------------------------------------------------------
-- XML parser (a scanner and a XML tree constructor)

parseXMLs :: String -> Either String [XML]
parseXMLs input = tokenizeXML input >>= buildXML

parseXML :: String -> Either String XML
parseXML input = do
   xmls <- parseXMLs input
   case xmls of
      [xml] -> return xml
      _     -> fail "multiple (or zero) xml objects at top-level"

-- helper data type for the scanner
data TokenXML = TokenOpen      String AttrList 
              | TokenClose     String 
              | TokenOpenClose String AttrList 
              | TokenText      String
   deriving Show

----------------------------------------------------------------
-- XML scanner

tokenizeXML :: String -> Either String [TokenXML]
tokenizeXML input = 
   case input of
      [] -> return []
      '<':'/':tl -> case break (not . isAlphaNum) tl of
                       (tag, '>':xs) -> do 
                          xmls <- tokenizeXML xs
                          return (TokenClose tag : xmls)
                       _ -> fail "expected a '>' when scanning a closing tag"
      '<':tl     -> do let (tag, xs) = break (not . isAlphaNum) tl
                       (attrs, ys) <- getAttrs (dropWhile isSpace xs)
                       case ys of
                          '/':'>':xs -> do 
                             xmls <- tokenizeXML xs
                             return (TokenOpenClose tag attrs : xmls)
                          '>':xs -> do
                             xmls <- tokenizeXML xs
                             return (TokenOpen tag attrs : xmls)
                          _ -> fail "expected a '/' or '>' when scanning an opening tag"
      _          -> do let (xs, ys) = break (=='<') input
                       xmls <- tokenizeXML ys
                       return $ [ TokenText (trim xs) | any (not . isSpace) xs ] ++ xmls

getAttrs :: String -> Either String (AttrList, String)
getAttrs xs = 
   case xs of
      hd:_ | isAlphaNum hd ->
         case break (not . isAlphaNum) xs of
            (key, '=':xs) -> do
               (value, ys) <- getString xs
               (attrs, zs) <- getAttrs (dropWhile isSpace ys)
               return ((key, value):attrs, zs)
            _ -> fail "expected a '=' when scanning an attribute"
      _ -> return ([], xs)

getString :: String -> Either String (String, String)
getString xs =
   let msg = "unexpected end of input when scanning a string" 
   in case xs of
         '"':rest -> case break (=='"') rest of
                        (xs, _:ys) -> return (xs, ys)
                        _          -> fail msg
         _ -> fail msg 
                    
----------------------------------------------------------------
-- XML tree constructor (from tokens)

buildXML :: [TokenXML] -> Either String [XML]
buildXML tokens = do
   (xmls, _) <- buildWithClose Nothing tokens
   return xmls

buildWithClose :: Maybe String -> [TokenXML] -> Either String ([XML], [TokenXML])
buildWithClose expected tokens =
   case tokens of
      [] | isNothing expected -> return ([], []) 
         | otherwise          -> fail "unexpected end of input"
      TokenOpen tag attrs : rest -> do
         (xmls1, xs) <- buildWithClose (Just tag) rest
         (xmls2, ys) <- buildWithClose expected xs
         return (Tag tag attrs xmls1 : xmls2, ys)
      TokenClose tag : rest 
         | Just tag == expected -> return ([], rest)
         | otherwise -> fail $ "wrong tag" 
      TokenOpenClose tag attrs : rest -> do
         (xmls, xs) <- buildWithClose expected rest 
         return (Tag tag attrs [] : xmls, xs)
      TokenText text : rest -> do
         (xmls, xs) <- buildWithClose expected rest 
         return (Text text : xmls, xs)
         
----------------------------------------------------------------
-- XML pretty printer

showXML :: XML -> String
showXML = unlines . rec
 where
   rec (Text s)           = [s]
   rec (Tag tag attrs xs) = tagAttr tag attrs (concatMap rec xs)

tagAttr :: String -> AttrList -> [String] -> [String]
tagAttr t attrs xs
   | null xs   = [tagWithAttrs openCloseTag t attrs]
   | otherwise = [tagWithAttrs openTag t attrs] ++ indent 2 xs ++ [closeTag t]

openTag, closeTag, openCloseTag :: String -> String
openTag      t = "<"  ++ t ++ ">"
closeTag     t = openTag ("/" ++ t)
openCloseTag t = openTag (t ++ "/")

tagWithAttrs :: (String -> String) -> String -> AttrList -> String
tagWithAttrs f t attrs = f (concat $ intersperse " " $ t : map g attrs)
 where g (x, y) = x ++ "=" ++ show y
 
indent :: Int -> [String] -> [String]
indent n = map (replicate n ' '++)

----------------------------------------------------------------
-- XML utility functions

children :: XML -> [XML]
children (Tag _ _ xs) = xs
children _            = []

extract :: String -> XML -> Maybe [XML]
extract n xml =
   case [ xs | Tag m _ xs <- children xml, n==m ] of
      [hd] -> return hd
      _    -> Nothing