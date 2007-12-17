module Common.Logging where

import Control.Concurrent
import Control.Monad
import System.Time

data LogConfig = LogConfig
   { logFile      :: String
   , logRetries   :: Int
   , logDelay     :: Int -- in micro-seconds
   , logTimeStamp :: Bool
   , logTracing   :: Bool -- trace messages
   }

defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig
   { logFile      = "logfile"
   , logRetries   = 5
   , logDelay     = 100000
   , logTimeStamp = True
   , logTracing   = False
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
      | otherwise = do 
              appendFile (logFile config) text
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