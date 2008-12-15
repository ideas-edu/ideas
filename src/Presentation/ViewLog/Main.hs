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
import System.Directory
import ViewLog.LogEntry

main :: IO ()
main = runCGI $ do
   fileField <- getInput "file"     -- read fields
   lineField <- getInput "line"
   server <- serverName
   script <- scriptName
   
   let self = "http://" ++ server ++ script
       mID  = case (fileField, lineField) of
                 (Just f, Just l) | all isDigit l && not (null l) -> 
                    Just (f, read l)
                 _ -> Nothing
   
   setHeader "Content-type" "text/html"
   case mID of
      Just entryID -> do
         e <- lift $ getEntry entryID
         output $ showHTML $ entryToHTML e
      Nothing -> do
         infos <- lift readMetaInfos
         output $ showHTML $ (infosToHTML self infos)

readMetaInfos :: IO [MetaInfo]
readMetaInfos = do
   files <- getDirectoryContents "."
   let logFiles = filter (".log" `isSuffixOf`) files
   lists <- mapM readIndexFile logFiles
   return (concat lists)
   
---------------------------------------------------------
  
groupByDay :: [MetaInfo] -> [[MetaInfo]]
groupByDay = groupBy (\x y -> f x == f y)
 where
   f i = (day (timeStamp i), month (timeStamp i))
  
-------------------------------------------------------------------------

infosToHTML :: String -> [MetaInfo] -> HTML
infosToHTML self infos = htmlPage "Logs"
   (mapM_ formatGroup list)
 where
   list  = groupByDay infos
   formatGroup xs@(info:_) = do
      h2 (date (timeStamp info) ++ " (" ++ show (length xs) ++ ")")
      table (map makeRow xs)
   formatGroup _ = return ()
   makeRow info = 
      [ let params = "?file=" ++ (fst $ entryID info) ++ "&line=" ++ (show $ snd $ entryID info)
        in link (self ++ params) (text $ time $ timeStamp info)
      , text $ service info
      , text $ strategy info
      , text $ mode info
      , text $ ip info
      ]
   date ts = unwords 
      [ weekday ts
      , month ts
      , show (day ts)
      , show (year ts)
      ]

entryToHTML :: Entry -> HTML
entryToHTML entry = htmlPage "Log" $ do
   let info = metaInfo entry
   h1 "Information"
   ul [ text $ show (timeStamp info)
      , text $ "IP address: " ++ ip info
      ]
   h1 "Request"
   preText (unlines $ request entry)
   h1 "Reply"
   preText (unlines $ reply entry)