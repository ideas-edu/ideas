module Main (main) where

import OpenMath.LAServer
import OpenMath.Interactive
import Network.CGI
import System.Environment
import Common.Logging
import Data.Maybe

main :: IO ()
main = do 
   args <- getArgs
   case args of 
      [] -> runCGI cgiMain
      ["--test", file]     -> readFile file >>= putStrLn . respond . Just
      ["--html", file]     -> readFile file >>= putStrLn . respondHTML
      ["--oneliner", file] -> readFile file >>= putStrLn . oneliner
      _ -> putStrLn $ unlines   
              [ "laservice.cgi (version " ++ versionNr ++ ")"   
              , "   use with --test     [request file] for testing"
              , "   use with --oneliner [request file] to output the request as a 'one-liner'"
              ]
      
cgiMain :: CGI CGIResult
cgiMain = do 
   input <- getInput "input"             -- read matrix xml string 
   setHeader "Content-type" "text/plain" -- return plain text
   addr <- remoteAddr -- the IP address of the remote host making the request
   let answer = respondHTML input
   logMsg $ unlines [addr, fromMaybe "" input, answer]
   output answer 

logMsg :: String -> CGI ()
logMsg = liftIO . logMessageWith defaultLogConfig {logFile = "laservice.log"}

oneliner :: String -> String
oneliner = (url++) . unwords . concatMap words . lines
 where url = "http://ideas.cs.uu.nl/cgi-bin/laservice.cgi?input="