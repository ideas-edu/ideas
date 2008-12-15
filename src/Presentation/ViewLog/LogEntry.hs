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
-- Log entries
--
-----------------------------------------------------------------------------
module ViewLog.LogEntry 
   ( Logs, EntryID, Entry(..), TimeStamp(..), MetaInfo(..)
   , readLogFile, getEntry, readIndexFile
   ) where

import Data.Char
import Data.List
import Data.Maybe
import System.Directory
import System.Time

type Logs = [Entry]

type EntryID = (String, Int)

data Entry = Entry
   { metaInfo  :: MetaInfo
   , request   :: [String]
   , reply     :: [String]
   }
   
data TimeStamp = TimeStamp
   { weekday :: String
   , month   :: String
   , day     :: Int
   , time    :: String
   , zone    :: String
   , year    :: Int
   }

data MetaInfo = MetaInfo 
   { entryID   :: EntryID
   , timeStamp :: TimeStamp
   , ip        :: String
   , service   :: String
   , strategy  :: String
   , mode      :: String
   }

instance Show TimeStamp where
   show ts = unwords $ map ($ ts)
      [weekday, month, show . day, time, zone, show . year]
      
----------------------------------------------------------------------
-- Reading log files

readLogFile :: String -> IO Logs
readLogFile file = do
   xs <- readFile file
   return $ makeEntries file $ zip [1..] (lines xs)
 
makeEntries :: String -> [(Int, String)] -> [Entry]
makeEntries file = rec . dropWhile (not . isHeaderLine . snd)
 where
   rec [] = []
   rec ((linenr,hd):rest) =
      case bracketed hd of
         Just (a, b) -> Entry 
            { metaInfo = MetaInfo 
                 { entryID   = (file, linenr)
                 , timeStamp = toTimeStamp a
                 , ip        = drop 13 b
                 , service   = fromMaybe "" (getService req)
                 , strategy  = fromMaybe "" (getStrategy req)
                 , mode      = fromMaybe "" (getMode req)
                 }
            , request   = req
            , reply     = repl
            } : rec ys
         _ -> error "readLogFile"
    where
      (xs, ys) = break (isHeaderLine . snd) rest
      (req, repl) = splitBlock [] (map snd xs)
   
   splitBlock acc xs@(hd:tl) 
      | not (isStartOfReply xs) = 
           splitBlock (hd:acc) tl
   splitBlock acc xs = (reverse acc, xs)
  
-- for example, [Tue Sep  2 09:15:59 CEST 2008] IP address: 145.20.45.86
isHeaderLine :: String -> Bool
isHeaderLine ('[':rest) = take 13 (dropWhile (/=']') rest) == "] IP address:"
isHeaderLine _ = False

bracketed :: String -> Maybe (String, String)
bracketed ('[':rest) =
   case break (==']') rest of 
      (xs, _:ys) -> return (xs, ys)
      _          -> Nothing
bracketed _ = Nothing

-- <reply      or     {"result"
isStartOfReply :: [String] -> Bool
isStartOfReply = p . filter (not . isSpace) . concat
 where
   p s = "<reply" `isPrefixOf` s || "{\"result\"" `isPrefixOf` s
    
toTimeStamp :: String -> TimeStamp
toTimeStamp txt =
   case words txt of
      [a, b, c, d, e, f] | all isDigit (c++f) -> 
         TimeStamp a b (read c) d e (read f)
      _ -> error $ "Invalid timestamp: " ++ txt
         
getService :: [String] -> Maybe String
getService = rec . filter (not . isSpace) . concat
 where
   rec xs@(_:tl) 
      | "\"method\":\"" `isPrefixOf` xs =
           Just (takeWhile (/='"') $ drop 10 xs)
      | "service=\"" `isPrefixOf` xs =
           Just (takeWhile (/='"') $ drop 9 xs)
      | otherwise = rec tl
   rec [] = Nothing

getMode :: [String] -> Maybe String
getMode = rec . filter (not . isSpace) . concat
 where
   rec ('{':_) = Just "JSON"
   rec ('<':_) = Just "XML"
   rec _       = Nothing

getStrategy :: [String] -> Maybe String
getStrategy = rec . filter (not . isSpace) . concat
 where
   rec xs@(_:tl)
      | "<strategy>" `isPrefixOf` xs =
           Just (takeWhile (/='<') $ drop 10 xs)
      | "\"params\":[\"" `isPrefixOf` xs =
           Just (takeWhile (/='"') $ drop 11 xs)
      | "\"params\":[[\"" `isPrefixOf` xs =
           Just (takeWhile (/='"') $ drop 12 xs)
      | otherwise = rec tl
   rec [] = Nothing

----------------------------------------------------------------------
-- Reading one log entry

getEntry :: EntryID -> IO Entry
getEntry (file, linenr) = do
   input <- readFile file
   let xs = dropWhile (not . isHeaderLine . snd) $ drop (linenr-1) $ zip [1..] $ lines input
   case makeEntries file xs of
      e:_ -> return e
      _   -> fail "Invalid entry ID"

----------------------------------------------------------------------
-- Writing index files

makeIndexFile :: String -> [MetaInfo] -> IO ()
makeIndexFile file infos = do 
   modTime <- getModificationTime file
   time    <- getClockTime 
   writeFile (file ++ ".index") $ unlines $
        ("-- Index file generated on " ++ show time)
      : show modTime
      : map formatMetaInfo infos

formatMetaInfo :: MetaInfo -> String
formatMetaInfo info = concat $ intersperse "," 
   [ show $ snd $ entryID info
   , show $ timeStamp info
   , ip info
   , service info
   , strategy info
   , mode info
   ]

----------------------------------------------------------------------
-- Reading index files

readIndexFile :: String -> IO [MetaInfo]
readIndexFile file = do 
   modTime <- getModificationTime file
   xs <- readFile (file ++ ".index") `catch` (\_ -> return [])
   case lines xs of
      _:mt:rest | mt == show modTime -> 
         return $ catMaybes $ map readInfoLine rest
      _ -> do
         logs <- readLogFile file
         let infos = map metaInfo logs
         makeIndexFile file infos
         return infos

 where 
   readInfoLine :: String -> Maybe MetaInfo
   readInfoLine txt =
      case uncomma txt of
         [a,b,c,d,e,f] -> Just $ 
            MetaInfo (file, read a) (toTimeStamp b) c d e f
         _ -> Nothing

   uncomma :: String -> [String]
   uncomma [] = []
   uncomma xs = let (ys, zs) = break (==',') xs
                in ys : uncomma (drop 1 zs)