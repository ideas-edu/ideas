{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- Copyright 2015, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Facilities to create a log database
--
-----------------------------------------------------------------------------
--  $Id$

module Ideas.Main.Logging 
   ( Record(..), makeRecord, addRequest
   , logEnabled, logRecord
   ) where

import Data.Maybe
import Data.Time
import Ideas.Service.Request (Request, Schema(..))
import qualified Ideas.Service.Request as R

#ifdef DB
import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)
#endif

type Diff = NominalDiffTime
type Time = UTCTime

data Record = Record
   { service      :: String  -- name of feedback service
   , exerciseid   :: String  -- exercise identifier
   , userid       :: String  -- user identifier (e.g. student number)
   , source       :: String  -- tool/learning environment that makes request
   , ipaddress    :: String  -- IP address of client
   , binary       :: String  -- name of (cgi) binary that is being executed
   , dataformat   :: String  -- xml, json
   , encoding     :: String  -- options for encoding (e.g. OpenMath, string)
   , time         :: Time    -- date and time of request
   , responsetime :: Diff    -- time needed for processing request
   , ruleid       :: String  -- rule identifier (customized for each service)
   , serviceinfo  :: String  -- summary of reply (customized for each service)
   , version      :: String  -- version (and revision) information
   , errormsg     :: String  -- internal error message (default: empty string)
   , input        :: String  -- raw input (request)
   , output       :: String  -- raw output (reply)
   }
 deriving Show

record :: Record
record = Record "" "" "" "" "" "" "" "" t0 0 "" "" "" "" "" ""
 where t0 = UTCTime (toEnum 0) 0

makeRecord :: IO Record
makeRecord = do
   now <- getCurrentTime
   return record { time = now }

addRequest :: Request -> Record -> Record
addRequest req r = r 
   { service    = maybe "unknown" show (R.serviceId req)
   , exerciseid = maybe "unknown" show (R.exerciseId req)
   , source     = fromMaybe "unknown" (R.source req)
   , binary     = fromMaybe "" (R.cgiBinary req)
   , dataformat = show (R.dataformat req)
   , encoding   = show (R.encoding req)
   }

--------------------------------------------------------------------------------

logEnabled :: Bool
logRecord  :: Schema -> Record -> IO ()

#ifdef DB
logEnabled = True
logRecord schema r = 
   case schema of
      V1 -> connectSqlite3 "service.db"  >>= logRecordWith V1 r
      V2 -> connectSqlite3 "requests.db" >>= logRecordWith V2 r
      NoLogging -> return ()
#else
-- without logging
logEnabled    = False
logRecord _ _ = return ()
#endif

--------------------------------------------------------------------------------

#ifdef DB
nameOfTable :: Schema -> String
nameOfTable V1 = "log"
nameOfTable _  = "requests"

columnsInTable :: Schema -> Record -> [SqlValue]
columnsInTable V1 = values_v1
columnsInTable _  = values_v2

values_v1 :: Record -> [SqlValue]
values_v1 r =
   let get f = toSql (f r) 
   in [ get service, get exerciseid, get source, get dataformat, get encoding
      , get input, get output, get ipaddress, get time, get responsetime
      ]
      
values_v2 :: Record -> [SqlValue]
values_v2 r =
   let get f = toSql (f r) 
   in [ get service, get exerciseid, get userid, get source, get ipaddress
      , get dataformat, get encoding, get time, get responsetime, get ruleid
      , get serviceinfo, get version, get errormsg, get input, get output
      ]
      
logRecordWith :: IConnection c => Schema -> Record -> c -> IO ()
logRecordWith schema r conn = do
   -- calculate duration
   end <- getCurrentTime
   let diff = diffUTCTime end (time r)
   -- insert data into database
   insertRecord schema r {responsetime = diff} conn
   -- close the connection to the database
   disconnect conn
 `catchSql` \err -> 
   putStrLn $ "Error in logging to database: " ++ show err

insertRecord :: IConnection c => Schema -> Record -> c ->  IO ()
insertRecord schema r conn =
   let cols = columnsInTable schema r
       pars = "(" ++ intercalate "," (replicate (length cols) "?") ++ ")"
       stm  = "INSERT INTO " ++ nameOfTable schema ++ " VALUES " ++ pars
   in run conn stm cols >> commit conn
#endif
