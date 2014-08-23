{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
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
--  $Id$

module Ideas.Main.Default
   ( defaultMain, defaultCGI, newDomainReasoner
     -- extra exports
   , Some(..), serviceList, metaServiceList, Service
   , module Ideas.Service.DomainReasoner
   ) where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Time
import Ideas.Common.Id
import Ideas.Common.Utils (useFixedStdGen, Some(..))
import Ideas.Common.Utils.TestSuite
import Ideas.Encoding.ModeJSON (processJSON)
import Ideas.Encoding.ModeXML (processXML)
import Ideas.Main.BlackBoxTests
import Ideas.Main.Documentation
import Ideas.Main.LoggingDatabase
import Ideas.Main.Options hiding (fullVersion)
import Ideas.Service.DomainReasoner
import Ideas.Service.FeedbackScript.Analysis
import Ideas.Service.Request
import Ideas.Service.ServiceList
import Ideas.Service.Types (Service)
import Network.CGI
import Prelude hiding (catch)
import System.IO
import System.IO.Error (ioeGetErrorString)
import qualified Ideas.Main.Options as Options

defaultMain :: DomainReasoner -> IO ()
defaultMain dr = do
   flags <- getFlags
   if null flags
      then defaultCGI dr
      else defaultCommandLine dr flags

-- Invoked as a cgi binary
defaultCGI :: DomainReasoner -> IO ()
defaultCGI dr = do
   startTime <- getCurrentTime
   logRef    <- newIORef (return ())
   runCGI $ do
      addr   <- remoteAddr       -- the IP address of the remote host making the request
      cgiBin <- scriptName       -- get name of binary
      raw    <- getInput "input" -- read input
      input  <- case raw of
                   Just s  -> return s
                   Nothing -> do 
                      b <- acceptsHTML
                      if b then return defaultBrowser else 
                         fail "environment variable 'input' is empty"
      (req, txt, ctp) <- liftIO $ process dr (Just cgiBin) input
      -- save logging action for later
      when (useLogging req) $
         liftIO $ writeIORef logRef $
            logMessage req input txt addr startTime
      writeHeader ctp
      output txt
   -- log request to database
   join (readIORef logRef)
   -- if something goes wrong
 `catch` \ioe -> runCGI $ do
   writeHeader "text/plain"
   output ("Invalid request: " ++ ioeGetErrorString ioe)

writeHeader :: String -> CGI ()
writeHeader ctp = do
   setHeader "Content-type" ctp
   -- Cross-Origin Resource Sharing (CORS) prevents browser warnings
   -- about cross-site scripting
   setHeader "Access-Control-Allow-Origin" "*"

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
               input <- hGetContents h
               (_, txt, _) <- process dr Nothing input
               putStrLn txt
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

process :: DomainReasoner -> Maybe String -> String -> IO (Request, String, String)
process dr cgiBin input =
   case discoverDataFormat input of
      Just XML  -> processXML (Just 5) dr cgiBin input
      Just JSON -> processJSON (Just 5) (isJust cgiBin) dr input
      _ -> fail "Invalid input"

newDomainReasoner :: IsId a => a -> DomainReasoner
newDomainReasoner a = mempty
   { reasonerId  = newId a
   , version     = shortVersion
   , fullVersion = Options.fullVersion
   }