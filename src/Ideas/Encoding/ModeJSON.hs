-----------------------------------------------------------------------------
-- Copyright 2016, Ideas project team. This file is distributed under the
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
import Data.Maybe
import Ideas.Common.Library hiding (exerciseId)
import Ideas.Utils.Prelude (timedSeconds)
import Ideas.Encoding.DecoderJSON
import Ideas.Encoding.Options (Options, makeOptions, maxTime, cgiBin)
import Ideas.Encoding.EncoderJSON
import Ideas.Encoding.Evaluator
import Ideas.Encoding.Logging (LogRef, changeLog, errormsg)
import Ideas.Service.DomainReasoner
import Ideas.Encoding.Request
import Ideas.Text.JSON

processJSON :: Options -> DomainReasoner -> LogRef -> String -> IO (Request, String, String)
processJSON options dr logRef txt = do
   json <- either fail return (parseJSON txt)
   req  <- jsonRequest options json
   resp <- jsonRPC json $ \fun arg ->
              maybe id timedSeconds (maxTime options) (myHandler options dr logRef req fun arg)
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
   p c = not (isAlphaNum c || isSpace c || c `elem` ".-_")

addVersion :: String -> JSON -> JSON
addVersion str json =
   case json of
      Object xs -> Object (xs ++ [info])
      _         -> json
 where
   info = ("version", String str)

jsonRequest :: Monad m => Options -> JSON -> m Request
jsonRequest options json = do
   let exId = lookupM "params" json >>= extractExerciseId
   srv  <- stringOption  "method"      json newId
   src  <- stringOption  "source"      json id
   rinf <- stringOption  "requestinfo" json id
   seed <- stringOptionM "randomseed"  json (defaultSeed options) (return . readM)
   enc  <- stringOptionM "encoding"    json [] readEncoding
   sch  <- stringOptionM "logging"     json Nothing (fmap Just . readSchema)
   return mempty
      { serviceId   = srv
      , exerciseId  = exId
      , source      = src
      , cgiBinary   = cgiBin options
      , requestInfo = rinf
      , logSchema   = sch
      , randomSeed  = seed
      , dataformat  = Just JSON
      , encoding    = enc
      }

-- Use a fixed seed for random number generation for command-line invocations
defaultSeed :: Options -> Maybe Int
defaultSeed options
   | isJust (cgiBin options) = Nothing
   | otherwise = Just 2805 -- magic number

stringOption :: Monad m => String -> JSON -> (String -> a) -> m (Maybe a)
stringOption attr json f = stringOptionM attr json Nothing (return . Just . f)

stringOptionM :: Monad m => String -> JSON -> a -> (String -> m a) -> m a
stringOptionM attr json a f =
   case lookupM attr json of
      Just (String s) -> f s
      Just _  -> fail $ "Invalid value for " ++ attr ++ " (expecting string)"
      Nothing -> return a

myHandler :: Options -> DomainReasoner -> LogRef -> Request -> RPCHandler
myHandler opt1 dr logRef request fun json = do
   srv <- findService dr (newId fun)
   Some ex <- case exerciseId request of
                 Just a  -> findExercise dr a
                 Nothing -> return (Some emptyExercise)
   opt2 <- makeOptions dr ex request
   let options = opt1 <> opt2
   evalService logRef ex options jsonEvaluator srv json

jsonEvaluator :: Evaluator a JSON JSON
jsonEvaluator = Evaluator jsonDecoder jsonEncoder