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
-- Services using JSON notation
--
-----------------------------------------------------------------------------

module Ideas.Encoding.ModeJSON (processJSON) where

import Control.Monad
import Data.Char
import Ideas.Common.Library hiding (exerciseId)
import Ideas.Common.Utils (Some(..), timedSeconds)
import Ideas.Encoding.DecoderJSON
import Ideas.Encoding.Encoder (makeOptions)
import Ideas.Encoding.EncoderJSON
import Ideas.Encoding.Evaluator
import Ideas.Main.Logging (LogRef, changeLog, errormsg)
import Ideas.Service.DomainReasoner
import Ideas.Service.Request
import Ideas.Text.JSON

processJSON :: Maybe Int -> Maybe String -> DomainReasoner -> LogRef -> String -> IO (Request, String, String)
processJSON maxTime cgiBin dr logRef input = do
   json <- either fail return (parseJSON input)
   req  <- jsonRequest cgiBin json
   resp <- jsonRPC json $ \fun arg ->
              maybe id timedSeconds maxTime (myHandler dr logRef req fun arg)
   unless (responseError resp == Null) $
      changeLog logRef (\r -> r {errormsg = show (responseError resp)})
   let f   = if compactOutput req then compactJSON else show
       out = addVersion (version dr) (toJSON resp)
   return (req, f out, "application/json")

-- TODO: Clean-up code
extractExerciseId :: Monad m => JSON -> m Id
extractExerciseId json =
   case json of
      String s -> return (newId s)
      Array [String _, String _, a@(Array _)] -> extractExerciseId a
      Array [String _, String _, _, a@(Array _)] -> extractExerciseId a
      Array (String s:tl) | any p s -> extractExerciseId (Array tl)
      Array (hd:_) -> extractExerciseId hd
      _ -> fail "no code"
 where
   p c = not (isAlphaNum c || isSpace c || c `elem` ".-")

addVersion :: String -> JSON -> JSON
addVersion str json =
   case json of
      Object xs -> Object (xs ++ [info])
      _         -> json
 where
   info = ("version", String str)

jsonRequest :: Monad m => Maybe String -> JSON -> m Request
jsonRequest cgiBin json = do
   let exId = lookupM "params" json >>= extractExerciseId
   srv  <- stringOption  "method"      json newId
   src  <- stringOption  "source"      json id
   rinf <- stringOption  "requestinfo" json id
   enc  <- stringOptionM "encoding"    json [] readEncoding
   sch  <- stringOptionM "logging"     json Nothing (liftM Just . readSchema)
   return emptyRequest
      { serviceId   = srv
      , exerciseId  = exId
      , source      = src
      , cgiBinary   = cgiBin
      , requestInfo = rinf
      , logSchema   = sch
      , dataformat  = JSON
      , encoding    = enc
      }

stringOption :: Monad m => String -> JSON -> (String -> a) -> m (Maybe a)
stringOption attr json f = stringOptionM attr json Nothing (return . Just . f)

stringOptionM :: Monad m => String -> JSON -> a -> (String -> m a) -> m a
stringOptionM attr json a f =
   case lookupM attr json of
      Just (String s) -> f s
      Just _  -> fail $ "Invalid value for " ++ attr ++ " (expecting string)"
      Nothing -> return a

myHandler :: DomainReasoner -> LogRef -> Request -> RPCHandler
myHandler dr logRef request fun json = do
   srv <- findService dr (newId fun)
   Some options <- makeOptions dr request
   evalService logRef options jsonEvaluator srv json

jsonEvaluator :: Evaluator a JSON JSON
jsonEvaluator = Evaluator jsonDecoder jsonEncoder