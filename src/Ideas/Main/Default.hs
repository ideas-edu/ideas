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
import Data.IORef
import Data.Time
import Ideas.Common.Id
import Ideas.Documentation.Make
import Ideas.Main.LoggingDatabase
import Ideas.Main.Options hiding (scriptDir, fullVersion)
import qualified Ideas.Main.Options as Options
import Network.CGI
import Ideas.Service.DomainReasoner
import Ideas.Service.FeedbackScript.Analysis
import Ideas.Service.ModeJSON (processJSON)
import Ideas.Service.ModeXML (processXML)
import Ideas.Service.Request
import System.IO

defaultMain :: DomainReasoner -> IO ()
defaultMain = extendDR $ \dr -> do
   startTime <- getCurrentTime
   flags     <- serviceOptions
   logRef    <- newIORef (return ())

   case withInputFile flags of
      -- from file
      Just file -> do
         when (FixRNG `elem` flags)
            useFixedStdGen -- use a predictable "random" number generator
         input    <- readFile file
         (req, txt, _) <- process dr input
         when (Logging True `elem` flags) $
            writeIORef logRef $ -- save logging action for later
               logMessage req input txt "local" startTime
         hSetBinaryMode stdout True
         putStrLn txt

      -- documentation mode
      _ | documentationMode flags ->
             let f = makeDocumentation dr (docDir flags) (testDir flags)
             in mapM_ f (docItems flags)

      -- feedback script options
        | scriptMode flags -> 
             withScripts dr (Just (Options.scriptDir flags))
                            [ a | MakeScriptFor a <- flags ]
                            [ a | AnalyzeScript a <- flags ]

      -- cgi binary
      Nothing -> runCGI $ do
         addr  <- remoteAddr           -- the IP address of the remote host making the request
         raw   <- getInput "input"     -- read input
         input <- case raw of
                     Nothing -> fail "Invalid request: environment variable \"input\" is empty"
                     Just s  -> return s
         (req, txt, ctp) <- liftIO $ process dr input
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

process :: DomainReasoner -> String -> IO (Request, String, String)
process dr input = do
   case discoverDataFormat input of
      Just XML  -> processXML  dr input
      Just JSON -> processJSON dr input
      _ -> fail "Invalid input"
      
extendDR :: (DomainReasoner -> IO ()) -> DomainReasoner -> IO ()
extendDR f dr = do
   flags <- serviceOptions
   f dr { scriptDirs  = [Options.scriptDir flags]
        }
        
newDomainReasoner :: IsId a => a -> DomainReasoner      
newDomainReasoner a = mempty 
   { reasonerId  = newId a 
   , version     = shortVersion
   , fullVersion = Options.fullVersion
   }