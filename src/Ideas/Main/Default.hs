-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Main module for feedback services
--
-----------------------------------------------------------------------------

module Ideas.Main.Default
   ( defaultMain, defaultCGI
     -- extra exports
   , Some(..), serviceList, metaServiceList, Service
   , module Ideas.Service.DomainReasoner
   ) where

import Control.Exception
import Control.Monad
import Data.Maybe
import Ideas.Common.Utils (useFixedStdGen, Some(..))
import Ideas.Common.Utils.TestSuite
import Ideas.Encoding.ModeJSON (processJSON)
import Ideas.Encoding.ModeXML (processXML)
import Ideas.Main.BlackBoxTests
import Ideas.Main.Documentation
import Ideas.Main.Options hiding (fullVersion)
import Ideas.Service.DomainReasoner
import Ideas.Service.FeedbackScript.Analysis
import Ideas.Service.Request
import Ideas.Service.ServiceList
import Ideas.Service.Types (Service)
import Network.CGI
import System.IO
import System.IO.Error (ioeGetErrorString)
import qualified Ideas.Main.Logging as Log

defaultMain :: DomainReasoner -> IO ()
defaultMain dr = do
   flags <- getFlags
   if null flags
      then defaultCGI dr
      else defaultCommandLine dr flags

-- Invoked as a cgi binary
defaultCGI :: DomainReasoner -> IO ()
defaultCGI dr = runCGI $ handleErrors $ do
   -- create a record for logging
   logRef  <- liftIO Log.newLogRef
   -- query environment
   addr    <- remoteAddr       -- the IP address of the remote host
   cgiBin  <- scriptName       -- get name of binary
   input   <- inputOrDefault
   -- process request
   (req, txt, ctp) <- liftIO $
      process dr logRef (Just cgiBin) input
   -- log request to database
   when (useLogging req) $ liftIO $ do
      Log.changeLog logRef $ \r -> Log.addRequest req r
         { Log.ipaddress = addr
         , Log.version   = shortVersion
         , Log.input     = input
         , Log.output    = txt
         }
      Log.logRecord (getSchema req) logRef

   -- write header and output
   setHeader "Content-type" ctp
   -- Cross-Origin Resource Sharing (CORS) prevents browser warnings
   -- about cross-site scripting
   setHeader "Access-Control-Allow-Origin" "*"
   output txt

inputOrDefault :: CGI String
inputOrDefault = do
   inHtml <- acceptsHTML
   ms     <- getInput "input" -- read variable 'input'
   case ms of
      Just s -> return s
      Nothing
         | inHtml    -> return defaultBrowser
         | otherwise -> fail "environment variable 'input' is empty"
 where
   -- Invoked from browser
   defaultBrowser :: String
   defaultBrowser = "<request service='index' encoding='html'/>"

   acceptsHTML :: CGI Bool
   acceptsHTML = do
      maybeAcceptCT <- requestAccept
      let htmlCT = ContentType "text" "html" []
          xs = negotiate [htmlCT] maybeAcceptCT
      return (isJust maybeAcceptCT && not (null xs))

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
         InputFile file ->
            withBinaryFile file ReadMode $ \h -> do
               logRef <- liftIO Log.newLogRef
               input  <- hGetContents h
               (req, txt, _) <- process dr logRef Nothing input
               putStrLn txt
               when (PrintLog `elem` flags) $ do
                  Log.changeLog logRef $ \r -> Log.addRequest req r
                     { Log.ipaddress = "command-line"
                     , Log.version   = shortVersion
                     , Log.input     = input
                     , Log.output    = txt
                     }
                  Log.printLog logRef
         -- blackbox tests
         Test dir -> do
            tests  <- blackBoxTests dr dir
            result <- runTestSuiteResult True tests
            printSummary result
         -- generate documentation pages
         MakePages dir ->
            makeDocumentation dr dir
         -- feedback scripts
         MakeScriptFor s    -> makeScriptFor dr s
         AnalyzeScript file -> parseAndAnalyzeScript dr file
         PrintLog           -> return ()

process :: DomainReasoner -> Log.LogRef -> Maybe String -> String -> IO (Request, String, String)
process dr logRef cgiBin input = do
   format <- discoverDataFormat input
   run format (Just 5) cgiBin dr logRef input
 `catch` \ioe -> do
   let msg = "Error: " ++ ioeGetErrorString ioe
   Log.changeLog logRef (\r -> r { Log.errormsg = msg })
   return (emptyRequest, msg, "text/plain")
 where
   run XML  = processXML
   run JSON = processJSON
