{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
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
module Ideas.Main.Default (defaultMain, newDomainReasoner) where

import Ideas.Common.Utils (useFixedStdGen)
import Control.Monad
import Ideas.Main.BlackBoxTests
import Data.Maybe
import Data.IORef
import Data.Time
import Ideas.Common.Id
import Ideas.Common.Utils.TestSuite
import Ideas.Main.LoggingDatabase
import Ideas.Main.Options hiding (fullVersion)
import qualified Ideas.Main.Options as Options
import Network.CGI
import Ideas.Service.DomainReasoner
import Ideas.Service.FeedbackScript.Analysis
import Ideas.Encoding.ModeJSON (processJSON)
import Ideas.Encoding.ModeXML (processXML)
import Ideas.Main.Documentation
import Ideas.Service.Request
import System.IO
import System.IO.Error (ioeGetErrorString)
import Control.Exception
import Prelude hiding (catch)

defaultMain :: DomainReasoner -> IO ()
defaultMain dr = do
   startTime <- getCurrentTime
   flags     <- getFlags
   if null flags
      then defaultCGI dr startTime
      else defaultCommandLine dr flags

-- Invoked as a cgi binary
defaultCGI :: DomainReasoner -> UTCTime -> IO ()
defaultCGI dr startTime = do
   logRef <- newIORef (return ())
   runCGI $ do
      addr   <- remoteAddr       -- the IP address of the remote host making the request
      cgiBin <- scriptName       -- get name of binary
      raw    <- getInput "input" -- read input
      input  <- case raw of
                   Nothing -> fail "Invalid request: environment variable \"input\" is empty"
                   Just s  -> return s
      (req, txt, ctp) <- liftIO $ process dr (Just cgiBin) input
      -- save logging action for later
      unless (encoding req == Just HTMLEncoding) $ 
         liftIO $ writeIORef logRef $
            logMessage req input txt addr startTime
      setHeader "Content-type" ctp
      -- Cross-Origin Resource Sharing (CORS) prevents browser warnings
      -- about cross-site scripting
      setHeader "Access-Control-Allow-Origin" "*"
      output txt 
   -- log request to database
   join (readIORef logRef)
   -- if something goes wrong
 `catch` \ioe -> runCGI $ do
   setHeader "Content-type" "text/plain"
   setHeader "Access-Control-Allow-Origin" "*"
   output ("Invalid request\n" ++ ioeGetErrorString ioe)
   
-- Invoked from command-line with flags
defaultCommandLine :: DomainReasoner -> [Flag] -> IO ()
defaultCommandLine dr flags = do
   hSetBinaryMode stdout True
   useFixedStdGen -- always use a predictable "random" number generator
   mapM_ doAction flags
 where
   doAction flag =
      case flag of
         -- information
         Version -> putStrLn ("IDEAS, " ++ versionText)
         Help    -> putStrLn helpText
         -- process input file
         InputFile file -> do
            input <- readFile file
            (_, txt, _) <- process dr Nothing input
            putStrLn txt
         -- blackbox tests
         Test dir -> do
            tests  <- blackBoxTests dr dir
            result <- runTestSuiteResult tests
            printSummary result
         -- generate documentation pages
         MakePages dir -> 
            makeDocumentation dr dir
         -- feedback scripts
         MakeScriptFor s    -> makeScriptFor dr s
         AnalyzeScript file -> parseAndAnalyzeScript dr file

process :: DomainReasoner -> Maybe String -> String -> IO (Request, String, String)
process dr cgiBin input = do
   case discoverDataFormat input of
      Just XML  -> processXML dr cgiBin input
      Just JSON -> processJSON (isJust cgiBin) dr input
      _ -> fail "Invalid input"
        
newDomainReasoner :: IsId a => a -> DomainReasoner      
newDomainReasoner a = mempty 
   { reasonerId  = newId a 
   , version     = shortVersion
   , fullVersion = Options.fullVersion
   }