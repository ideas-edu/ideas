-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Facilities to create a log file
--
-----------------------------------------------------------------------------
module Common.Logging where

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Time

-- | A data type to configure the logging facility. The messages are logged in a file,
-- and in case an exception is thrown during the logging, a second attempt can be made
data LogConfig = LogConfig
   { logFile      :: String    -- ^ The file name
   , logRetries   :: Int       -- ^ The number of retries
   , logDelay     :: Int       -- ^ The delay between attempts (in micro-seconds)
   , logTimeStamp :: Bool      -- ^ Whether or not to include a time stamp
   , logTracing   :: Bool      -- ^ Flag to trace the logged messages
   }

-- | A default configuration for logging
defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig
   { logFile      = "logfile"
   , logRetries   = 5
   , logDelay     = 100000
   , logTimeStamp = True
   , logTracing   = False
   }

-- | Logs a message with the default configuration
logMessage :: String -> IO ()
logMessage = logMessageWith defaultLogConfig

-- | Logs a message using the supplied configuration
logMessageWith :: LogConfig -> String -> IO ()
logMessageWith config msg = 
  getCurrentTime >>= try (logRetries config)
 where 
   try :: Int -> UTCTime -> IO ()
   try n time 
      | n==0      = putStrLn $ "Log failed at " ++ show time
      | otherwise = do 
              appendFile (logFile config) (text ++ "\n")
              when (logTracing config) $ 
                 putStrLn $ "Log succeeded at " ++ show time
           `catch` \_ -> do
              when (logTracing config) $
                 putStrLn $ "Log attempt failed. Remaining attempts: " ++ show (n-1)
              threadDelay (logDelay config)
              try (n-1) time
    where
      text | logTimeStamp config = "[" ++ show time ++ "] " ++ msg
           | otherwise           = msg

logAction :: String -> IO a -> IO a
logAction = logActionWith defaultLogConfig
           
logActionWith :: LogConfig -> String -> IO a -> IO a
logActionWith config msg action = do
   begin <- getCurrentTime
   a     <- action
   end   <- getCurrentTime
   let diff   = diffUTCTime end begin 
       newcfg = config {logTimeStamp = False}
   logMessageWith newcfg ("[" ++ show diff ++ "] " ++ msg)
   return a
