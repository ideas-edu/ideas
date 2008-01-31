module Main (main) where

import OpenMath.LAServer
import Network.CGI
import System.Environment
import Common.Logging
import Data.Maybe

main :: IO ()
main = do 
   args <- getArgs
   case args of 
      [] -> runCGI cgiMain
      ["--test", file] -> readFile file >>= putStrLn . respond . Just
      _ -> putStrLn $ "laservice.cgi (version " ++ versionNr ++ ")\n   use with --test [request file] for testing"
      
cgiMain :: CGI CGIResult
cgiMain = do 
   input <- getInput "input"             -- read matrix xml string 
   setHeader "Content-type" "text/plain" -- return plain text
   addr <- remoteAddr -- the IP address of the remote host making the request
   let answer = respond input
   logMsg $ unlines [addr, fromMaybe "" input, answer]
   output answer 

logMsg :: String -> CGI ()
logMsg = liftIO . logMessageWith defaultLogConfig {logFile = "laservice.log"}