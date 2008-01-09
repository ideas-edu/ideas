module Main (main) where

import OpenMath.LAServer
import Network.CGI
import System.Environment
import Common.Logging

main :: IO ()
main = do 
   args <- getArgs
   case args of 
      [] -> runCGI cgiMain
      ["--test", file] -> do xs <- readFile file; putStrLn (respond xs)
      _ -> putStrLn "laservice.cgi: use with --test [request file] for testing"
      
cgiMain :: CGI CGIResult
cgiMain = do 
   input <- getInput "input"             -- read matrix xml string 
   setHeader "Content-type" "text/plain" -- return plain text
   case input of
      Nothing  -> output ("Invalid request.")
      Just xml -> do 
         addr <- remoteAddr -- the IP address of the remote host making the request
         let answer = respond xml
         logMsg $ unlines [addr, xml, answer]
         output answer 

logMsg :: String -> CGI ()
logMsg = liftIO . logMessageWith defaultLogConfig {logFile = "laservice.log"}
