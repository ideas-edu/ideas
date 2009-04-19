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
module OpenMath.ContentDictionary where

import OpenMath.Object
import OpenMath.XML
import Data.Char
import Data.List
import Data.Maybe
import System.Directory

main :: IO ()
main = do
   let base = "lib/Dictionaries"
       f x  = base ++ "/" ++ x
   xs   <- findOCDs base
   mcds <- mapM (readContentDictionary . f) xs
   let cds  = catMaybes mcds 
       defs = concatMap definitions cds 
   putStrLn $ show (length cds) ++ " valid dictionaries, with " ++ show (length defs) ++ " definitions"

   -- print [ p | d <- defs, p <- formalProperties d ]

findOCDs :: String -> IO [FilePath]
findOCDs filepath = do
   xs <- getDirectoryContents filepath
   return $ filter (".ocd" `isSuffixOf`) xs

readContentDictionary :: String -> IO (Maybe ContentDictionary)
readContentDictionary filename = do
   putStrLn $ "reading " ++ show filename ++ "..."
   input <- readFile filename
   case parseXML input of
      Left s  -> err s
      Right xml@(Tag tag _ _) | map toUpper tag == "CD" -> 
         case buildContentDictionary xml of 
            Left s -> err s
            Right cd -> do
               putStrLn $ "  found " ++ show (length $ definitions cd) ++ " definition(s)"
               return (Just cd)
      Right _   -> err "Not an OpenMath Content Description (ocd) file"
 `catch` \exception -> err (show exception)
 where
   err s = putStrLn ("   ERROR: " ++ show s) >> return Nothing

buildContentDictionary :: XML -> Either String ContentDictionary
buildContentDictionary xml = do
   name      <- extractText "CDName"       xml
   descr     <- extractText "Description"  xml
   revision  <- extractDate "CDDate"       xml
   review    <- extractDate "CDReviewDate" xml
   major     <- extractInt "CDVersion"     xml
   minor     <- extractInt "CDRevision"    xml
   theStatus <- extractStatus              xml
   let theBase = extractText "CDBase"      xml
   theURL    <- extractText "CDURL"        xml
   defs      <- mapM buildDefinition [ xml | xml@(Tag "CDDefinition" _ _) <- children xml ]
   return CD
      { dictionaryName = name
      , description    = descr
      , revisionDate   = revision
      , reviewDate     = review
      , versionNumber  = (major, minor)
      , status         = theStatus
      , base           = theBase
      , url            = theURL
      , definitions    = defs
  }

buildDefinition :: XML -> Either String Definition
buildDefinition xml = do
   theName    <- extractText "Name"        xml
   descr      <- extractText "Description" xml
   let theRole = extractText "Role"        xml
       cmps    = [ s     | xml@(Tag "CMP" _ [Text s]) <- children xml ]
       fmps    = [ this  | xml@(Tag "FMP" _ [this]) <- children xml ]
       exs     = [ these | xml@(Tag "Example" _ these) <- children xml ]
   return Definition
      { symbolName          = theName
      , symbolDescription   = descr
      , role                = theRole
      , commentedProperties = cmps
      , formalProperties    = map (either error id . xml2omobj) fmps
      , examples            = exs
      } 

extractDate :: String -> XML -> Either String Date
extractDate s xml = do
   txt <- extractText s xml
   case txt of 
      [y1,y2,y3,y4,'-',m1,m2,'-',d1,d2] | all isDigit [y1,y2,y3,y4,m1,m2,d1,d2] -> 
         return (read [y1,y2,y3,y4], read [m1,m2], read [d1,d2])
      _ -> fail ("invalid date (YYYY-MM-DD): " ++ txt)

extractInt :: String -> XML -> Either String Int
extractInt s xml = do 
   txt <- extractText s xml
   case reads txt of 
      [(n, xs)] | all isSpace xs -> 
         return n
      _ -> fail ("invalid number" ++ txt)

extractStatus :: XML -> Either String ContentDictionaryStatus
extractStatus xml = do
   txt <- extractText "CDStatus" xml
   let (hd, tl) = splitAt 1 txt
   case reads (map toUpper hd ++ map toLower tl) of
      [(st, xs)] | all isSpace xs -> 
         return st
      _ -> fail ("invalid status: " ++ txt)

data ContentDictionary = CD 
   { dictionaryName :: String
   , description    :: String
   , revisionDate   :: Date
   , reviewDate     :: Date
   , versionNumber  :: VersionNumber
   , status         :: ContentDictionaryStatus
   , base           :: Maybe String
   , url            :: String
   , definitions    :: [Definition]   
   } deriving Show

type VersionNumber = (Int, Int) -- major and minor part
type Date = (Int, Int, Int) -- YYYY-MM-DD

data ContentDictionaryStatus = Official | Experimental | Private | Obsolete deriving (Read,Show)

data Definition = Definition 
   { symbolName          :: String
   , symbolDescription   :: String
   , role                :: Maybe String
   , commentedProperties :: [String]
   , formalProperties    :: [OMOBJ]
   , examples            :: [[XML]]
   } deriving Show