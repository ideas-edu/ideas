module Main (main) where

import OpenMath.LAServer
import Network.CGI
import System.Environment

main :: IO ()
main = do 
   args <- getArgs
   case args of 
      [] -> runCGI cgiMain
      ["--test", file] -> do xs <- readFile file; putStrLn (respond xs)
      _ -> putStrLn "laservice.cgi: use with --test [request file] for testing"
      
cgiMain :: CGI CGIResult
cgiMain = do 
   input <- getInput "matrix"                -- read matrix xml string 
   setHeader "Content-type" "text/plain" -- return plain text
   case input of
      Just xml -> output (respond xml)
      Nothing  -> output ("Invalid request.")
