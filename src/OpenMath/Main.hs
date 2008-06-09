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
module Main (main) where

import Common.Utils (stringToHex)
import OpenMath.LAServer
import OpenMath.Interactive
import OpenMath.StrategyTable
import Network.CGI
import System.Environment
import Common.Logging
import Data.Maybe
import Data.Char

main :: IO ()
main = do 
   args <- getArgs
   case args of 
      [] -> runCGI cgiMain
      ["--test", file]     -> readFile file >>= putStrLn . respond . Just . convert
      ["--html", file]     -> readFile file >>= putStrLn . respondHTML (defaultURL True)
      ["--oneliner", file] -> readFile file >>= putStrLn . (defaultURL False++) . oneliner
      _ -> putStrLn $ unlines   
              [ "laservice.cgi (version " ++ versionNr ++ ")"   
              , "   use with --test     [request file] for testing"
              , "   use with --html     [request file] to output an html file with links for interaction"
              , "   use with --oneliner [request file] to output the request as a 'one-liner'"
              ]
      
cgiMain :: CGI CGIResult
cgiMain = do
   -- get input
   raw  <- getInput "input"     -- read matrix xml string
   mode <- getInput "mode"      -- optional: a mode
   addr <- remoteAddr           -- the IP address of the remote host making the request
   
   -- Convert escaped characters ('%')
   let input = fmap convert raw
   
   -- process input, log request (optional), and produce output
   case mode of
      
      Just "html" -> do
         server <- serverName
         script <- scriptName
         let self = "http://" ++ server ++ script ++ "?mode=html&input="
         logMsg $ addr ++ " (html mode)"
         setHeader "Content-type" "text/html" -- return html text
         output $ respondHTML self $ fromMaybe "" input
   
      _ -> do
         let answer = respond input
         logMsg $ unlines [addr, fromMaybe "" input, answer]
         setHeader "Content-type" "text/plain" -- return plain text
         output answer 

logMsg :: String -> CGI ()
logMsg = liftIO . logMessageWith defaultLogConfig {logFile = "laservice.log"}

convert :: String -> String
convert [] = []
convert ('%':c1:c2:cs) =
   case stringToHex [c1, c2] of
      Just i  -> chr i : convert cs
      Nothing -> '%' : convert (c1:c2:cs)
convert (c:cs) = c : convert cs