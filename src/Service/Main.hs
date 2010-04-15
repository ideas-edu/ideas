-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Main module for feedback services
--
-----------------------------------------------------------------------------
module Main (main) where

-- import Common.Logging
import Common.Utils (useFixedStdGen)
import Service.Options
import Service.ModeXML  (processXML)
import Service.ModeJSON (processJSON)
import Service.Request
import Service.LoggingDatabase
import Documentation.Make
import Network.CGI
import Control.Monad.Trans
import Control.Monad
import Data.IORef
import Data.Time

main :: IO ()
main = do
   startTime <- getCurrentTime
   flags     <- serviceOptions
   logRef    <- newIORef (return ())
   
   case withInputFile flags of      
      -- from file
      Just file -> do  
         when (FixRNG `elem` flags) 
            useFixedStdGen -- use a predictable "random" number generator
         input    <- readFile file
         (req, txt, _) <- process input
         when (Logging True `elem` flags) $ 
            writeIORef logRef $ -- save logging action for later
               logMessage req input txt "local" startTime
         putStrLn txt

      -- documentation mode
      _ | documentationMode flags ->
         makeDocumentation (docItems flags)

      -- cgi binary
      Nothing -> runCGI $ do
         addr  <- remoteAddr           -- the IP address of the remote host making the request          
         raw   <- getInput "input"     -- read input
         input <- case raw of
                     Nothing -> fail "Invalid request: environment variable \"input\" is empty"
                     Just s  -> return s
         (req, txt, ctp) <- lift $ process input
         lift $ writeIORef logRef $ -- save logging action for later
            logMessage req input txt addr startTime
         setHeader "Content-type" ctp
         output txt
   
   -- log request to database
   when (withLogging flags) $
      join (readIORef logRef)
   
process :: String -> IO (Request, String, String)
process input =
   case discoverDataFormat input of
      Just XML  -> processXML  input
      Just JSON -> processJSON input
      _         -> fail "Invalid input"