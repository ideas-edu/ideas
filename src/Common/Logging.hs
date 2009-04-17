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
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)


-- | A data type for choosing where to log, i.e. in a file or a database. The database
-- constructor accepts an wrapped database connection. In this way a connection can be
-- reused for efficiency reasons. At the moment our service isn't a (persistent) daemon,
-- so the code could have been simpler (ie. open db, do query, commit, close db). But it
-- is bad to be prepared ;-) | 
data LogDest = File String | Sqlite3 (IO ConnWrapper)

-- | A function to open a database connection. In case of an empty database, the table will
-- be generated.
openDB :: FilePath -> LogDest
openDB db = Sqlite3 $ do conn <- connectSqlite3 db
                         tables <- getTables conn
                         let createStmt = "CREATE TABLE log (time VARCHAR(80) NOT NULL, msg VARCHAR(250))"
                         if not (elem "log" tables) then run conn createStmt [] else return 0
                         return (ConnWrapper conn)

-- | A database connection *has* to be closed when finished using it.
closeDB :: LogConfig -> IO ()
closeDB cfg = case logDest cfg of
                Sqlite3 db -> db >>= disconnect
                _          -> return ()

-- | A data type to configure the logging facility. The messages are logged in a file,
-- and in case an exception is thrown during the logging, a second attempt can be made
data LogConfig = LogConfig
   { logDest      :: LogDest   -- ^ The logging destination, e.g. file or database
   , logRetries   :: Int       -- ^ The number of retries
   , logDelay     :: Int       -- ^ The delay between attempts (in micro-seconds)
   , logTimeStamp :: Bool      -- ^ Whether or not to include a time stamp
   , logTracing   :: Bool      -- ^ Flag to trace the logged messages
   }

-- | A default configuration for logging
defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig
   { logDest      = File "service.log" -- openDB "service.db"
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
   do time <- getCurrentTime
      try (logRetries config) time
 where 
   try :: Int -> UTCTime -> IO ()
   try n time 
      | n==0      = putStrLn $ "Log failed at " ++ show time
      | otherwise = do 
              append (logDest config) text
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
      append dest text = case dest of
                           File s     -> appendFile s (text ++ "\n")
                           Sqlite3 db -> db >>= (\conn -> run conn logStmt [toSql time, toSql text]
                                            >> commit conn)
        where
          logStmt = "INSERT INTO log VALUES (?,?)"

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
