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
-- Helper CGI script to view entries in the log files
--
-----------------------------------------------------------------------------
module Main (main) where

import Control.Monad.State
import Data.Char
import Data.List
import Data.Maybe
import Network.CGI
import Service.HTML

logfile :: String
logfile = "service1.log"

main :: IO ()
main = runCGI $ do
   idField <- getInput "id"     -- read "id" field
   server <- serverName
   script <- scriptName
  
   entries <- lift $ readLogFile logfile
   let self = "http://" ++ server ++ script
       mID  = case reads (fromMaybe "" idField) of
                 [(i, xs)] | all isSpace xs -> return i
                 _ -> Nothing
   setHeader "Content-type" "text/html"
   output $ showHTML $
      case mID of
         Just i -> 
            entryToHTML (entries !! i)
         Nothing -> 
            logsToHTML self entries
         

type Logs = [Entry]

data TimeStamp = TimeStamp
   { weekday :: String
   , month   :: String
   , day     :: Int
   , time    :: String
   , zone    :: String
   , year    :: Int
   }

instance Show TimeStamp where
   show ts = unwords 
      [ time ts
      , "(" ++ month ts
      , show (day ts)
      , show (year ts) ++ ")"
      ]

toTimeStamp :: String -> TimeStamp
toTimeStamp txt =
   case words txt of
      [a, b, c, d, e, f] | all isDigit (c++f) -> 
         TimeStamp a b (read c) d e (read f)
      _ -> error $ "Invalid timestamp: " ++ txt

data Entry = Entry
   { timeStamp :: TimeStamp
   , ip        :: String
   , request   :: [String]
   , reply     :: [String]
   } deriving Show
   
readLogFile :: String -> IO Logs
readLogFile file = do
   xs <- readFile file
   return (makeEntries $ lines xs)

makeEntries :: [String] -> [Entry]
makeEntries = rec . dropWhile (not . isEntryLine)
 where
   rec [] = []
   rec (hd:rest) = case bracketed hd of
                      Just (a, b) -> Entry 
                         { timeStamp = toTimeStamp a
                         , ip        = drop 13 b
                         , request   = req
                         , reply     = repl
                         } : rec ys
                      _ -> error "makeEntries"
    where
      (xs, ys) = break isEntryLine rest
      (req, repl) = splitBlock [] xs
   
   splitBlock acc xs@(hd:tl) 
      | not (isStartOfReply xs) = 
           splitBlock (hd:acc) tl
   splitBlock acc xs = (reverse acc, xs)

-- for example, [Tue Sep  2 09:15:59 CEST 2008] IP address: 145.20.45.86
isEntryLine :: String -> Bool
isEntryLine ('[':rest) = take 13 (dropWhile (/=']') rest) == "] IP address:"
isEntryLine _ = False

bracketed :: String -> Maybe (String, String)
bracketed ('[':rest) =
   case break (==']') rest of 
      (xs, _:ys) -> return (xs, ys)
      _          -> Nothing
bracketed _ = Nothing

-- <reply 
-- {"result"
isStartOfReply :: [String] -> Bool
isStartOfReply = p . filter (not . isSpace) . concat
 where
   p s = "<reply" `isPrefixOf` s || "{\"result\"" `isPrefixOf` s

groupByDay :: [(a, Entry)] -> [[(a, Entry)]]
groupByDay = groupBy (\(_, x) (_, y) -> f x == f y)
 where
   f e = (day (timeStamp e), month (timeStamp e))

requestService :: Entry -> Maybe String
requestService = rec . filter (not . isSpace) . concat . request
 where
   rec xs@(_:tl) 
      | "\"method\":\"" `isPrefixOf` xs =
           Just (takeWhile (/='"') $ drop 10 xs)
      | "service=\"" `isPrefixOf` xs =
           Just (takeWhile (/='"') $ drop 9 xs)
      | otherwise = rec tl
   rec [] = Nothing

requestMode :: Entry -> Maybe String
requestMode = rec . filter (not . isSpace) . concat . request
 where
   rec ('{':_) = Just "JSON"
   rec ('<':_) = Just "XML"
   rec _       = Nothing

requestStrategy :: Entry -> Maybe String
requestStrategy = rec . filter (not . isSpace) . concat . request
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
  
-------------------------------------------------------------------------

logsToHTML :: String -> Logs -> HTML
logsToHTML self logs = htmlPage "Logs"
   (mapM_ formatGroup list)
 where
   list  = groupByDay (zip [0..] logs)
   formatGroup xs@((_, e):_) = do
      h2 (date (timeStamp e) ++ " (" ++ show (length xs) ++ ")")
      table (map makeRow xs)
   makeRow (n, e) = 
      [ link (self ++ "?id=" ++ show n) (text $ time $ timeStamp e)
      , text $ fromMaybe "" (requestService e)
      , text $ fromMaybe "" (requestStrategy e)
      , text $ fromMaybe "" (requestMode e)
      , text $ ip e
      ]
   date ts = unwords 
      [ weekday ts
      , month ts
      , show (day ts)
      , show (year ts)
      ]

entryToHTML :: Entry -> HTML
entryToHTML entry = htmlPage "Log" $ do
   h1 "Information"
   ul [ text $ show (timeStamp entry)
      , text $ "IP address: " ++ ip entry
      ]
   h1 "Request"
   preText (unlines $ request entry)
   h1 "Reply"
   preText (unlines $ reply entry)