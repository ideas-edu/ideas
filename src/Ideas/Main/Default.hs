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
module Ideas.Main.Default (defaultMain, ideasVersion) where

import Ideas.Common.Utils (useFixedStdGen)
import Control.Monad
import Data.IORef
import Data.Time
import Ideas.Documentation.Make
import Ideas.Main.LoggingDatabase
import Ideas.Main.Options
import Network.CGI
import Ideas.Service.DomainReasoner
import Ideas.Service.FeedbackScript.Analysis
import Ideas.Service.ModeJSON (processJSON)
import Ideas.Service.ModeXML (processXML)
import Ideas.Service.Request
import System.IO

ideasVersion :: String
ideasVersion = "X" -- automatically copied from ideas.cabal

defaultMain :: (forall a . DomainReasoner a -> IO a) -> IO ()
defaultMain run = do
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
         hSetBinaryMode stdout True
         putStrLn txt

      -- documentation mode
      _ | documentationMode flags ->
             run $
                let f = makeDocumentation (docDir flags) (testDir flags)
                in mapM_ f (docItems flags)

      -- feedback script options
        | scriptMode flags -> run $
             withScripts (Just (scriptDir flags))
                         [ a | MakeScriptFor a <- flags ]
                         [ a | AnalyzeScript a <- flags ]

      -- cgi binary
      Nothing -> runCGI $ do
         addr  <- remoteAddr           -- the IP address of the remote host making the request
         raw   <- getInput "input"     -- read input
         input <- case raw of
                     Nothing -> fail "Invalid request: environment variable \"input\" is empty"
                     Just s  -> return s
         (req, txt, ctp) <- liftIO $ process input
         liftIO $ writeIORef logRef $ -- save logging action for later
            logMessage req input txt addr startTime
         setHeader "Content-type" ctp
         -- Cross-Origin Resource Sharing (CORS) prevents browser warnings
         -- about cross-site scripting
         setHeader "Access-Control-Allow-Origin" "*"
         output txt

   -- log request to database
   when (withLogging flags) $
      join (readIORef logRef)

 where
   process :: String -> IO (Request, String, String)
   process input = run $
      case discoverDataFormat input of
         Just XML  -> processXML input
         Just JSON -> processJSON input
         _ -> fail "Invalid input"