module Main (main) where

import OpenMath.LAServer
import OpenMath.Interactive
import OpenMath.StrategyTable
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
   input <- getInput "input"     -- read matrix xml string
   mode  <- getInput "mode"      -- optional: a mode
   addr  <- remoteAddr           -- the IP address of the remote host making the request
   
   -- process input, log request (optional), and produce output
   case mode of
      
      Just "html" -> do
         setHeader "Content-type" "text/html" -- return html text
         server <- serverName
         script <- scriptName
         let self = "http://" ++ server ++ script ++ "?mode=html&input="
         output $ respondHTML self $ fromMaybe "" input
   
      _ -> do
         let answer = respond input
         logMsg $ unlines [addr, fromMaybe "" input, answer]
         setHeader "Content-type" "text/plain" -- return plain text
         output answer 

logMsg :: String -> CGI ()
logMsg = liftIO . logMessageWith defaultLogConfig {logFile = "laservice.log"}