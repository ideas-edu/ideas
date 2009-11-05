{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Facilities to create a log database
--
-----------------------------------------------------------------------------
module Service.LoggingDatabase (logMessage, logEnabled) where

import Data.Time
import Service.Request
-- import System.Posix.Syslog
import Data.Maybe
#ifdef DB
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)

logEnabled :: Bool
logEnabled = True

-- | Log a message to the database (a Sqlite database).
logMessage :: Request -> String -> String -> String -> UTCTime -> IO ()
logMessage req input output ipaddress begin = do 
     -- make a connection with the database
     conn <- connectSqlite3 "/tmp/service.db"

     -- check if the database exists, if not make one
     --tables <- getTables conn
     --if not (elem "log" tables) then run conn createStmt [] else return 0

     -- calculate duration
     end <- getCurrentTime
     let diff = diffUTCTime end begin 

     -- insert data into database
     run conn "INSERT INTO log VALUES (?,?,?,?,?,?,?,?,?,?,?)" 
             [ toSql $ service req
             , toSql $ maybe "unknown" show (exerciseID req)
             , toSql $ fromMaybe "unknown" (source req)
             , toSql $ show (dataformat req)
             , toSql $ maybe "unknown" show (encoding req)
             , toSql $ input
             , toSql $ output
             , toSql $ ipaddress
             , toSql $ begin
             , toSql $ diff
             , SqlNull
             ]
     commit conn

     -- close the connection to the database
     disconnect conn

  `catch` \err -> do 
    putStrLn $ "Error in logging to database: " ++ show err
--    withSyslog "service.cgi" [PID] LOCAL0 $ do 
--      syslog Debug $ "Error in logging to database: " ++ show err

{-
-- | Log table schema
createStmt =  "CREATE TABLE log ( service      VARCHAR(250)"
           ++                  ", exerciseID   VARCHAR(250)"
           ++                  ", source       VARCHAR(250)"
           ++                  ", dataformat   VARCHAR(250)"
           ++                  ", encoding     VARCHAR(250)"
           ++                  ", input        VARCHAR(250)"
           ++                  ", output       VARCHAR(250)"
           ++                  ", ipaddress    VARCHAR(20)"
           ++                  ", time         TIME"
           ++                  ", responsetime TIME"
           ++                  ", id           INTEGER PRIMARY KEY)"
-}
#else
logEnabled :: Bool
logEnabled = False

logMessage :: Request -> String -> String -> String -> UTCTime -> IO ()
logMessage _ _ _ _ _ = return ()
#endif

