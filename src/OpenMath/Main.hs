module Main (main) where

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

convert :: String -> String
convert [] = []
convert ('%':c1:c2:cs) =
   case stringToHex [c1, c2] of
      Just i  -> chr i : convert cs
      Nothing -> '%' : convert (c1:c2:cs)
convert (c:cs) = c : convert cs

stringToHex :: String -> Maybe Int
stringToHex = foldl op (Just 0)
 where
   op (Just i) c = fmap (\j -> i*16 + j) (charToHex c)
   op Nothing  _ = Nothing

charToHex :: Char -> Maybe Int
charToHex c
   | isDigit c = return (ord c - 48)
   | toUpper c `elem` ['A' .. 'F'] = return (ord (toUpper c) - 55)
   | otherwise = Nothing