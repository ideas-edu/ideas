{-# LANGUAGE CPP, FlexibleContexts #-}
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
-- Facilities to create a log database
--
-----------------------------------------------------------------------------

module Ideas.Encoding.Logging
   ( Record(..), addRequest, addState
   , LogRef, makeLogRef, defaultLogRef, enableLogging, disableLogging
   , changeLog, logEnabled, logRecord, logRecordWith, printLog
   , selectFrom
   , getRecord
   , getFilePath
   ) where

import Control.Monad
import Data.Char
import Data.IORef
import Data.Maybe
import Data.Time
import Ideas.Encoding.Request (Request, Schema(..))
import Ideas.Service.State
import qualified Ideas.Encoding.Request as R

#ifdef DB
import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3, setBusyTimeout)
#endif

type Diff = NominalDiffTime
type Time = UTCTime

-- | The Record datatype is based on the Ideas Request Logging Schema version 2.
data Record = Record
   { useLogging   :: Bool
     -- request attributes
   , service      :: String  -- name of feedback service
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
record = Record True "" "" "" "" "" "" "" "" "" "" t0 0 "" "" "" "" "" "" "" ""
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

data LogRef = NoRef | LogRef FilePath Schema (IORef Record)

instance Semigroup LogRef where
   NoRef <> r = r
   r <> _     = r

instance Monoid LogRef where
   mempty  = NoRef
   mappend = (<>)

defaultLogRef :: IO LogRef
defaultLogRef = makeLogRef "requests.db" V2

makeLogRef :: FilePath -> Schema -> IO LogRef
makeLogRef file schema = do
   r   <- makeRecord
   ref <- newIORef r
   return (LogRef file schema ref)

enableLogging :: LogRef -> IO ()
enableLogging = flip changeLog (\r -> r {useLogging = True})

disableLogging :: LogRef -> IO ()
disableLogging = flip changeLog (\r -> r {useLogging = False})

whenLogging :: LogRef -> IO () -> IO ()
whenLogging logRef m = do
   r <- getRecord logRef
   when (useLogging r) m

getRecord :: LogRef -> IO Record
getRecord NoRef          = return record
getRecord (LogRef _ _ r) = readIORef r

getFilePath :: LogRef -> Maybe FilePath
getFilePath NoRef = Nothing
getFilePath (LogRef fp _ _) = Just fp

changeLog :: LogRef -> (Record -> Record) -> IO ()
changeLog NoRef          _ = return ()
changeLog (LogRef _ _ r) f = modifyIORef r f

printLog :: LogRef -> IO ()
printLog logRef = do
   putStrLn "-- log information"
   getRecord logRef >>= print

--------------------------------------------------------------------------------

logEnabled :: Bool
logRecord  :: LogRef -> IO ()
selectFrom :: FilePath -> String -> [String] -> ([String] -> IO a) -> IO [a]

#ifdef DB
logEnabled = True
#else
-- without logging
logEnabled         = False
logRecord _        = return ()
selectFrom _ _ _ _ = return []

logRecordWith :: LogRef -> c -> IO ()
logRecordWith _ _ = return ()
#endif

--------------------------------------------------------------------------------

#ifdef DB
nameOfTable :: Schema -> String
nameOfTable V1 = "log"
nameOfTable _  = "requests"

columnsInTable :: Schema -> Record -> [SqlValue]
columnsInTable V1 = values_v1
columnsInTable _  = values_v2

-- Do not store ip addresses in database

values_v1 :: Record -> [SqlValue]
values_v1 r =
   let get f = toSql (f r)
   in [ get service, get exerciseid, get source, get dataformat, get encoding
      , get input, get output, SqlNull {- get ipaddress -}, get time, get responsetime
      ]

values_v2 :: Record -> [SqlValue]
values_v2 r =
   let get f = toSql (f r)
   in [ get service, get exerciseid, get source, get script, get requestinfo
      , get dataformat, get encoding, get userid, get sessionid, get taskid
      , get time, get responsetime, SqlNull {- get ipaddress -}, get binary, get version
      , get errormsg, get serviceinfo, get ruleid, get input, get output
      ]

logRecord NoRef  = return ()
logRecord logRef@(LogRef file _ _) =
   (whenLogging logRef) $ do
      -- connect to database
      conn <- connectSqlite3 file
      setBusyTimeout conn 200 -- milliseconds
      logRecordWith logRef conn
      -- close the connection to the database
      disconnect conn
    `catchSql` \_ ->
      return ()

logRecordWith :: IConnection c => LogRef -> c -> IO ()
logRecordWith NoRef _  = return ()
logRecordWith logRef@(LogRef _ schema _) conn = do
   -- calculate duration
   r   <- getRecord logRef
   end <- getCurrentTime
   let diff = diffUTCTime end (time r)
   -- insert data into database
   insertRecord schema r {responsetime = diff} conn
 `catchSql` \_ ->
   return ()

insertRecord :: IConnection c => Schema -> Record -> c ->  IO ()
insertRecord schema r conn =
   let cols = columnsInTable schema r
       pars = "(" ++ intercalate "," (replicate (length cols) "?") ++ ")"
       stm  = "INSERT INTO " ++ nameOfTable schema ++ " VALUES " ++ pars
   in run conn stm cols >> commit conn

selectFrom database table columns f = do
   let sql    = "SELECT " ++ commas (map safe columns) ++ " from " ++ safe table
       commas = intercalate ","
       safe   = filter isAlphaNum
   con  <- connectSqlite3 database
   stat <- prepare con sql
   _    <- execute stat []
   rows <- fetchAllRows stat
   xs   <- mapM (f . map fromSql) rows
   disconnect con
   return xs
#endif