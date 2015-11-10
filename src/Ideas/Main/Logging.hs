{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Facilities to create a log database
--
-----------------------------------------------------------------------------

module Ideas.Main.Logging
   ( Record(..), addRequest, addState
   , LogRef, newLogRef, noLogRef, changeLog
   , logEnabled, logRecord, printLog
   ) where

import Data.IORef
import Data.Maybe
import Data.Time
import Ideas.Service.Request (Request, Schema(..))
import Ideas.Service.State
import qualified Ideas.Service.Request as R

#ifdef DB
import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)
#endif

type Diff = NominalDiffTime
type Time = UTCTime

-- | The Record datatype is based on the Ideas Request Logging Schema version 2.
data Record = Record
   { -- request attributes
     service      :: String  -- name of feedback service
   , exerciseid   :: String  -- exercise identifier
   , source       :: String  -- tool/learning environment that makes request
   , script       :: String  -- name of feedback script (for textual feedback)
   , requestinfo  :: String  -- additional information from client (only for logging)
     -- request format
   , dataformat   :: String  -- xml, json
   , encoding     :: String  -- options for encoding (e.g. OpenMath, string)
   , -- grouping requests
     userid       :: String  -- user identifier (e.g. student number)
   , sessionid    :: String  -- session identifier (grouping requests for one task)
   , taskid       :: String  -- task identifier (default: start term)
     -- meta-information
   , time         :: Time    -- date and time of request
   , responsetime :: Diff    -- time needed for processing request
   , ipaddress    :: String  -- IP address of client
   , binary       :: String  -- name of (cgi) binary that is being executed
   , version      :: String  -- version (and revision) information
   , errormsg     :: String  -- internal error message (default: empty string)
     -- service info
   , serviceinfo  :: String  -- summary of reply (customized for each service)
   , ruleid       :: String  -- rule identifier (customized for each service)
     -- raw data
   , input        :: String  -- raw input (request)
   , output       :: String  -- raw output (reply)
   }
 deriving Show

record :: Record
record = Record "" "" "" "" "" "" "" "" "" "" t0 0 "" "" "" "" "" "" "" ""
 where t0 = UTCTime (toEnum 0) 0

makeRecord :: IO Record
makeRecord = do
   now <- getCurrentTime
   return record { time = now }

-- | Add record information from the Request datatype
addRequest :: Request -> Record -> Record
addRequest req r = r
   { service     = maybe (service r) show (R.serviceId req)
   , exerciseid  = maybe (exerciseid r) show (R.exerciseId req)
   , source      = fromMaybe (source r) (R.source req)
   , script      = fromMaybe (script r) (R.feedbackScript req)
   , requestinfo = fromMaybe (requestinfo r) (R.requestInfo req)
   , dataformat  = show (R.dataformat req)
   , encoding    = show (R.encoding req)
   , binary      = fromMaybe (binary r) (R.cgiBinary req)
   }

-- | Add record information from the state (userid, sessionid, taskid)
addState :: State a -> Record -> Record
addState st r = r
   { userid    = fromMaybe (userid r)    (stateUser st)
   , sessionid = fromMaybe (sessionid r) (stateSession st)
   , taskid    = fromMaybe (taskid r)    (stateStartTerm st)
   }

---------------------------------------------------------------------

newtype LogRef = L { mref :: Maybe (IORef Record) }

noLogRef :: LogRef
noLogRef = L Nothing

newLogRef :: IO LogRef
newLogRef = do
   r   <- makeRecord
   ref <- newIORef r
   return (L (Just ref))

getRecord :: LogRef -> IO Record
getRecord = maybe (return record) readIORef . mref

changeLog :: LogRef -> (Record -> Record) -> IO ()
changeLog = maybe (\_ -> return ()) modifyIORef . mref

printLog :: LogRef -> IO ()
printLog logRef = do
   putStrLn "-- log information"
   getRecord logRef >>= print

--------------------------------------------------------------------------------

logEnabled :: Bool
logRecord  :: Schema -> LogRef -> IO ()

#ifdef DB
logEnabled = True
logRecord schema logRef =
   case schema of
      V1 -> connectSqlite3 "service.db"  >>= logRecordWith V1 logRef
      V2 -> connectSqlite3 "requests.db" >>= logRecordWith V2 logRef
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
   in [ get service, get exerciseid, get source, get script, get requestinfo
      , get dataformat, get encoding, get userid, get sessionid, get taskid
      , get time, get responsetime, get ipaddress, get binary, get version
      , get errormsg, get serviceinfo, get ruleid, get input, get output
      ]

logRecordWith :: IConnection c => Schema -> LogRef -> c -> IO ()
logRecordWith schema logRef conn = do
   -- calculate duration
   r   <- getRecord logRef
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