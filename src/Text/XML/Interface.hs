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
-- Collection of common operation on XML documents
--
-----------------------------------------------------------------------------
module Text.XML.Interface where

import Control.Arrow
import Text.XML.Document (Name)
import Text.XML.Unicode (decoding)
import Text.XML.Parser (document, extParsedEnt)
import Text.XML.ParseLib (parse)
import qualified Text.XML.Document as D
import System.FilePath (takeDirectory, pathSeparator)
import Data.Char (chr, ord)
import Data.Maybe

data Element = Element
   { name       :: Name
   , attributes :: Attributes
   , content    :: Content
   } 

instance Show Element where
   show = show . extend

type Content = [Either String Element]

type Attributes = [Attribute]
data Attribute = Name := String

normalize :: D.XMLDoc -> Element
normalize doc = toElement (D.root doc)
 where
   toElement :: D.Element -> Element
   toElement (D.Element n as c) = 
      Element n (map toAttribute as) (toContent c)
   
   toAttribute :: D.Attribute -> Attribute
   toAttribute (n D.:= v) = 
      n := concatMap (either return refToString) v
   
   toContent :: D.Content -> Content
   toContent = merge . concatMap f
    where
      f :: D.XML -> Content
      f (D.Tagged e)    = [Right (toElement e)]
      f (D.CharData s)  = [Left s]
      f (D.CDATA s)     = [Left s]
      f (D.Reference r) = refToContent r
   
   refToString :: D.Reference -> String
   refToString (D.CharRef i)   = [chr i]
   refToString (D.EntityRef _) = "" -- error
   
   refToContent :: D.Reference -> Content
   refToContent (D.CharRef i)   = [Left [chr i]]
   refToContent (D.EntityRef s) = 
      fromJust (lookup s entities)

   entities :: [(String, Content)]
   entities = 
      [ (n, toContent (snd ext)) | (n, ext) <- D.externals doc ] ++ 
      -- predefined entities
      map (second (return . Left . return))
         [("lt",'<'), ("gt",'>'), ("amp",'&'), ("apos",'\''), ("quot",'"')]
   
   merge :: Content -> Content
   merge (Left s:Left t:rest) = merge (Left (s++t) : rest)
   merge (x:xs) = x:merge xs
   merge []     = []
   
extend :: Element -> D.XMLDoc
extend e = D.XMLDoc
   { D.versionInfo = Nothing
   , D.encoding    = Nothing
   , D.standalone  = Nothing
   , D.dtd         = Nothing
   , D.externals   = []
   , D.root        = toElement e
   }
 where
   toElement :: Element -> D.Element
   toElement (Element n as c) =
      D.Element n (map toAttribute as) (concatMap toXML c)
   
   toAttribute :: Attribute -> D.Attribute
   toAttribute (n := s) = (D.:=) n (map Left s)
   
   toXML :: Either String Element -> [D.XML]
   toXML = either fromString (return . D.Tagged . toElement)
   
   fromString :: String -> [D.XML]
   fromString [] = []
   fromString xs@(hd:tl) 
      | null xs1  = D.Reference (D.CharRef (ord hd)) : fromString tl
      | otherwise = D.CharData xs1 : fromString xs2
    where
      (xs1, xs2) = break ((> 127) . ord) xs

-----------------------------------------------------

parseXML :: String -> Either String Element
parseXML xs = do
   input <- decoding xs
   doc   <- parse document input
   return (normalize doc)

parseIO :: String -> IO Element
parseIO baseFile = do
   -- putStrLn $ "Reading " ++ show baseFile
   xs    <- readFile baseFile
   input <- decoding xs
   case parse document input of
      Left err  -> fail err
      Right doc -> do
         let exts = getExternals doc
         rs <- mapM (parseExternal . snd) exts
         let new = doc { D.externals = zip (map fst exts) rs }
         return (normalize new)

 where 
   getExternals :: D.XMLDoc -> [(String, String)]
   getExternals doc =
      case D.dtd doc of 
         Just (D.DTD _ _ decls) ->
            [ (n, s) | D.EntityDecl True n (Right (D.System s, Nothing)) <- decls ]  
         Nothing -> []
 
   parseExternal :: String -> IO D.External
   parseExternal extFile = do
      let full = takeDirectory baseFile ++ [pathSeparator] ++ extFile
      -- putStrLn $ "Reading " ++ show full
      xs    <- readFile full
      input <- decoding xs
      case parse extParsedEnt input of
         Right doc -> return doc
         Left err  -> fail err

-----------------------------------------------------

noAttributes :: Element -> Bool
noAttributes = null . attributes

findAttribute :: Monad m => String -> Element -> m String
findAttribute s (Element _ as _) =
   case [ t | n := t <- as, s==n ] of
      [hd] -> return hd
      _    -> fail $ "Invalid attribute: " ++ show s

findChild :: Monad m => String -> Element -> m Element
findChild s e = 
   case filter ((==s) . name) (children e) of
      [a] -> return a
      _   -> fail $ "Child not found: " ++ show s

children :: Element -> [Element]
children e = [ c | Right c <- content e ]

getData :: Element -> String
getData e = concat [ s | Left s <- content e ]

{-
children :: D.Element -> [D.Element]
children (D.Element _ _ c) = [ e | D.Tagged e <- c ]

getAttributes :: D.Element -> [(String, String)]
getAttributes (D.Element _ as _) = 
   [ (n, concatMap f av) | n D.:= av <- as ]
 where 
   f :: Either Char D.Reference -> String
   f (Left c)              = [c]
   f (Right (D.CharRef n))   = [chr $ fromIntegral n]
   f (Right (D.EntityRef _)) = []

findAttribute :: Monad m => String -> D.Element -> m String
findAttribute n e =
   case lookup n (getAttributes e) of
      Just a  -> return a
      Nothing -> fail $ "Attribute not found: " ++ show n

getData :: D.Element -> String
getData (D.Element _ _ c) = concat [ s | D.CharData s <- c ]
-}