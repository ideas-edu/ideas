-----------------------------------------------------------------------------
-- Copyright 2016, Ideas project team. This file is distributed under the
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
   ( defaultMain, defaultMainWith, defaultCGI
     -- extra exports
   , serviceList, metaServiceList, Service
   , module Ideas.Service.DomainReasoner
   ) where

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString, unpack)
import Data.Char
import Data.Maybe
import Data.String
import Ideas.Encoding.ModeJSON (processJSON)
import Ideas.Encoding.ModeXML (processXML)
import Ideas.Encoding.Options (Options, maxTime, optionCgiBin)
import Ideas.Encoding.Request
import Ideas.Main.CmdLineOptions hiding (fullVersion)
import Ideas.Service.DomainReasoner
import Ideas.Service.FeedbackScript.Analysis
import Ideas.Service.ServiceList
import Ideas.Service.Types (Service)
import Ideas.Utils.BlackBoxTests
import Ideas.Utils.TestSuite
import Ideas.Utils.Prelude
import Network.HTTP.Types
import Network.Wai hiding (Request)
import System.IO
import System.IO.Error (ioeGetErrorString)
import qualified Ideas.Encoding.Logging as Log
import qualified Ideas.Main.CmdLineOptions as Options
import qualified Network.Wai as CGI
import qualified Ideas.Main.CGI as CGI

defaultMain :: DomainReasoner -> IO ()
defaultMain = defaultMainWith mempty

defaultMainWith :: Options -> DomainReasoner -> IO ()
defaultMainWith options dr = do
   cmdLineOptions <- getCmdLineOptions
   if null cmdLineOptions
      then defaultCGI options dr
      else defaultCommandLine options (addVersion dr) cmdLineOptions

-- Invoked as a cgi binary
defaultCGI :: Options -> DomainReasoner -> IO ()
defaultCGI options dr = CGI.run $ \req respond -> do 
   -- create a record for logging
   logRef  <- Log.newLogRef
   -- query environment
   let script = fromMaybe "" (findHeader "CGI-Script-Name" req) -- get name of binary
       addr   = fromMaybe "" (findHeader "REMOTE_ADDR" req)     -- the IP address of the remote host
   input   <- inputOrDefault req
   -- process request
   (preq, txt, ctp) <-
      process (options <> optionCgiBin script) dr logRef input
   -- log request to database
   when (useLogging preq) $ do
      Log.changeLog logRef $ \r -> Log.addRequest preq r
         { Log.ipaddress = addr
         , Log.version   = shortVersion
         , Log.input     = input
         , Log.output    = txt
         }
      Log.logRecord (getSchema preq) logRef

   -- write header and output
   respond $ responseLBS
      status200
      [ (fromString "Content-Type", fromString ctp)
        -- Cross-Origin Resource Sharing (CORS) prevents browser warnings
        -- about cross-site scripting
      , (fromString "Access-Control-Allow-Origin", fromString "*")
      ]
      (fromString txt)

inputOrDefault :: CGI.Request -> IO String
inputOrDefault req =
   let query  = queryString req
       inHtml = acceptsHTML
   in case getInput query "input" of
         Just s -> return s
         Nothing
            | inHtml    -> return defaultBrowser
            | otherwise -> fail "environment variable 'input' is empty"
 where
   -- Invoked from browser
   defaultBrowser :: String
   defaultBrowser = "<request service='index' encoding='html'/>"

   acceptsHTML :: Bool
   acceptsHTML = "text/html" `elem` accepts req

-- Invoked from command-line with flags
defaultCommandLine :: Options -> DomainReasoner -> [CmdLineOption] -> IO ()
defaultCommandLine options dr cmdLineOptions = do
   hSetBinaryMode stdout True
   mapM_ doAction cmdLineOptions
 where
   doAction cmdLineOption =
      case cmdLineOption of
         -- information
         Version -> putStrLn ("IDEAS, " ++ versionText)
         Help    -> putStrLn helpText
         -- process input file
         InputFile file ->
            withBinaryFile file ReadMode $ \h -> do
               logRef <- Log.newLogRef
               input  <- hGetContents h
               (req, txt, _) <- process options dr logRef input
               putStrLn txt
               when (PrintLog `elem` cmdLineOptions) $ do
                  Log.changeLog logRef $ \r -> Log.addRequest req r
                     { Log.ipaddress = "command-line"
                     , Log.version   = shortVersion
                     , Log.input     = input
                     , Log.output    = txt
                     }
                  Log.printLog logRef
         -- blackbox tests
         Test dir -> do
            tests  <- blackBoxTests (makeTestRunner dr) ["xml", "json"] dir
            result <- runTestSuiteResult True tests
            printSummary result
         -- feedback scripts
         MakeScriptFor s    -> makeScriptFor dr s
         AnalyzeScript file -> parseAndAnalyzeScript dr file
         PrintLog           -> return ()

process :: Options -> DomainReasoner -> Log.LogRef -> String -> IO (Request, String, String)
process options dr logRef input = do
   format <- discoverDataFormat input
   run format options {maxTime = Just 5} (addVersion dr) logRef input
 `catch` \ioe -> do
   let msg = "Error: " ++ ioeGetErrorString ioe
   Log.changeLog logRef (\r -> r { Log.errormsg = msg })
   return (mempty, msg, "text/plain")
 where
   run XML  = processXML
   run JSON = processJSON

makeTestRunner :: DomainReasoner -> String -> IO String
makeTestRunner dr input = do
   (_, out, _) <- process mempty dr Log.noLogRef input
   return out

addVersion :: DomainReasoner -> DomainReasoner
addVersion dr = dr
   { version     = update version Options.shortVersion
   , fullVersion = update fullVersion Options.fullVersion
   }
 where
   update f s = if null (f dr) then s else f dr
   
-- local helper functions

findHeader :: String -> CGI.Request -> Maybe String
findHeader s = fmap fromByteString . lookup (fromString s) . requestHeaders

getInput :: Query -> String -> Maybe String
getInput query s = fmap fromByteString (join (lookup (fromString s) query))

accepts :: CGI.Request -> [String]
accepts = maybe [] (splitsWithElem ',') . findHeader "Accept"

fromByteString :: ByteString -> String
fromByteString = map (chr . fromEnum) . unpack