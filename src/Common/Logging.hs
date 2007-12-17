module Common.Logging where

import Control.Concurrent
import System.Time

data LogConfig = LogConfig
   { logFile      :: String
   , logRetries   :: Int
   , logDelay     :: Int -- in micro-seconds
   , logTimeStamp :: Bool 
   }

defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig
   { logFile      = "logfile"
   , logRetries   = 5
   , logDelay     = 100000
   , logTimeStamp = True
   }

logMessage :: String -> IO ()
logMessage = logMessageWith defaultLogConfig

logMessageWith :: LogConfig -> String -> IO ()
logMessageWith config msg = 
   do time <- getClockTime
      try (logRetries config) time
 where 
   try :: Int -> ClockTime -> IO ()
   try n time 
      | n==0      = putStrLn $ "Log failed at " ++ show time
      | otherwise = appendFile (logFile config) text
                       `catch` \_ -> threadDelay (logDelay config) >> try (n-1) time
    where
      text | logTimeStamp config = "[" ++ show time ++ "] " ++ msg
           | otherwise           = msg