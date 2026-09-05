{-# OPTIONS -Wno-deprecations #-}
-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
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
import Ideas.Encoding.NewModeJSON (processJSON)
import Ideas.Encoding.ModeXML (processXML)
import Ideas.Encoding.Options (Options, optionCgiBin, logRef)
import Ideas.Encoding.Request
import Ideas.Main.CmdLineOptions hiding (fullVersion)
import Ideas.Service.DomainReasoner
import Ideas.Service.FeedbackScript.Analysis
import Ideas.Service.ServiceList
import Ideas.Service.Types (Service)
import qualified Ideas.Text.UTF8 as UTF8
import Ideas.Text.XML.Unicode (decoding)
import qualified Ideas.Utils.BlackBoxTests as BB
import Ideas.Utils.Prelude
import Ideas.Utils.TestSuite
import Network.HTTP.Types
import System.IO
import qualified Ideas.Encoding.Logging as Log
import qualified Ideas.Main.CGI as CGI
import qualified Ideas.Main.CmdLineOptions as Options
import qualified Network.Wai as WAI

defaultMain :: DomainReasoner -> IO ()
defaultMain = defaultMainWith mempty

defaultMainWith :: Options -> DomainReasoner -> IO ()
defaultMainWith options dr = do
   -- create a record for logging (use only if not already provided)
   ref <- Log.defaultLogRef
   let newOptions = options {logRef = logRef options <> ref}
   -- inspect command-line options
   cmdLineOptions <- getCmdLineOptions
   if null cmdLineOptions
      then defaultCGI newOptions dr
      else defaultCommandLine newOptions (addVersion dr) cmdLineOptions

-- Invoked as a cgi binary
defaultCGI :: Options -> DomainReasoner -> IO ()
defaultCGI options dr = CGI.run $ \req respond -> do
   -- query environment
   let script = fromMaybe "" (findHeader "CGI-Script-Name" req) -- get name of binary
       addr   = ""                                              -- no IP address of the remote host (GDPR)
   input   <- inputOrDefault req >>= decodingIO
   -- process request
   (preq, txt, ctp) <-
      process (optionCgiBin script options) dr input
   -- store request in log reference
   Log.changeLog (logRef options) $ \r -> Log.addRequest preq r
      { Log.ipaddress = addr
      , Log.version   = shortVersion
      , Log.input     = input
      , Log.output    = fromMaybe txt (UTF8.decode txt)
      }
   -- log request to database
   when (useLogging preq) $
      Log.logRecord (logRef options)
   -- write header and output
   respond $ WAI.responseLBS
      status200
      [ (fromString "Content-Type", fromString ctp)
        -- Cross-Origin Resource Sharing (CORS) prevents browser warnings
        -- about cross-site scripting
      , (fromString "Access-Control-Allow-Origin", fromString "*")
      ]
      (fromString txt)

inputOrDefault :: WAI.Request -> IO String
inputOrDefault req = do
   maybeInput <- inputFromRequest req
   case maybeInput of
      Just s -> return s
      Nothing
         | acceptsHTML -> return defaultBrowser
         | otherwise   -> fail "environment variable 'input' is empty"
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
         Rerun database ->
            processDatabase dr database
         InputFile file ->
            withBinaryFile file ReadMode $ \h -> do
               input  <- hGetContents h >>= decodingIO
               (req, txt, _) <- process options dr input
               putStrLn txt
               when (PrintLog `elem` cmdLineOptions) $ do
                  Log.changeLog (logRef options) $ \r -> Log.addRequest req r
                     { Log.ipaddress = "command-line"
                     , Log.version   = shortVersion
                     , Log.input     = input
                     , Log.output    = fromMaybe txt (UTF8.decode txt)
                     }
                  Log.printLog (logRef options)
         -- blackbox tests
         Test dir -> do
            let mode = if Interactive `elem` cmdLineOptions then BB.Interactive else BB.Report
            tests  <- BB.blackBoxTests (makeTestRunner dr) mode ["xml", "json"] dir
            result <- runTestSuiteResult True tests
            printSummary result
         -- feedback scripts
         MakeScriptFor s    -> makeScriptFor dr s
         AnalyzeScript file -> parseAndAnalyzeScript dr file
         _                  -> return ()

processDatabase :: DomainReasoner -> FilePath -> IO ()
processDatabase dr database = do
   (n, time) <- getDiffTime $ do
      rows <- Log.selectFrom database "requests" ["input"] $ \row -> do
         (_, out, _) <- process mempty dr (head row)
         putStrLn out
      return (length rows)
   putStrLn $ "processed " ++ show n ++ " requests in " ++ show time

process :: Options -> DomainReasoner -> String -> IO (Request, String, String)
process options dr input = do
   format <- discoverDataFormat input
   run format options (addVersion dr) input
 `catch` \e -> do
   let msg = "Error: " ++ show (e :: SomeException)
   Log.changeLog (logRef options) (\r -> r { Log.errormsg = msg })
   return (mempty, msg, "text/plain")
 where
   run XML  = processXML
   run JSON = processJSON

makeTestRunner :: DomainReasoner -> String -> IO String
makeTestRunner dr = decodingIO >=> fmap snd3 . process mempty dr

decodingIO :: String -> IO String
decodingIO = maybe (fail "unicode decoding failed") return . decoding

addVersion :: DomainReasoner -> DomainReasoner
addVersion dr = dr
   { version     = update version Options.shortVersion
   , fullVersion = update fullVersion Options.fullVersion
   }
 where
   update f s = if null (f dr) then s else f dr

-- local helper functions

findHeader :: String -> WAI.Request -> Maybe String
findHeader s = fmap fromByteString . lookup (fromString s) . WAI.requestHeaders

inputFromRequest :: WAI.Request -> IO (Maybe String)
inputFromRequest req =
   -- first try query string (for GET requests) ...
   case inputFromQuery (WAI.queryString req) of
      Just s  -> return (Just s)
      Nothing -> do
         -- ... then try request body (for POST requests)
         body <- WAI.requestBody req
         return (inputFromQuery (parseQuery body))

inputFromQuery :: Query -> Maybe String
inputFromQuery = fmap fromByteString . join . lookup (fromString "input")

accepts :: WAI.Request -> [String]
accepts = maybe [] (splitsWithElem ',') . findHeader "Accept"

fromByteString :: ByteString -> String
fromByteString = map (chr . fromEnum) . unpack