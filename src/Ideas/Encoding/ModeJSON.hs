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
-- Services using JSON notation
--
-----------------------------------------------------------------------------

module Ideas.Encoding.ModeJSON (processJSON, processJSON') where

import Control.Monad
import Data.Char
import Data.Maybe
import Ideas.Common.Library hiding (exerciseId)
import Ideas.Encoding.DecoderJSON
import Ideas.Encoding.EncoderJSON
import Ideas.Encoding.Evaluator
import Ideas.Encoding.Logging (changeLog, errormsg)
import Ideas.Encoding.Options (Options, makeOptions, maxTime, cgiBin, logRef)
import Ideas.Encoding.Request
import Ideas.Service.DomainReasoner
import Ideas.Text.JSON
import Ideas.Utils.Prelude (timedSeconds)

processJSON :: Options -> DomainReasoner -> String -> IO (Request, String, String)
processJSON options dr txt = do
   json <- either fail return (parseJSON txt)
   processJSON' options dr json

processJSON' :: Options -> DomainReasoner -> JSON -> IO (Request, String, String)
processJSON' options dr json = do
   req  <- jsonRequest options json
   resp <- jsonRPC json $ \fun arg ->
              maybe id timedSeconds (maxTime options) (myHandler options dr req fun arg)
   unless (responseError resp == Null) $
      changeLog (logRef options) (\r -> r {errormsg = show (responseError resp)})
   let f   = if compactOutput req then compactJSON else show
       out = addVersion (version dr) (toJSON resp)
   return (req, f out, "application/json")

-- TODO: Clean-up code
extractExerciseId :: JSON -> Maybe Id
extractExerciseId json =
   case json of
      String s -> return (newId s)
      Array [String _, String _, a@(Array _)] -> extractExerciseId a
      Array [String _, String _, _, a@(Array _)] -> extractExerciseId a
      Array (String s:tl) | any p s -> extractExerciseId (Array tl)
      Array (hd:_) -> extractExerciseId hd
      _ -> Nothing
 where
   p c = not (isAlphaNum c || isSpace c || c `elem` ".-_")

addVersion :: String -> JSON -> JSON
addVersion str json =
   case json of
      Object xs -> Object (xs ++ [info])
      _         -> json
 where
   info = ("version", String str)

jsonRequest :: Options -> JSON -> IO Request
jsonRequest options json = do
   let exId = (either (const Nothing) Just $ lookupM "params" json) >>= extractExerciseId
   srv  <- stringOption  "method"      json newId
   src  <- stringOption  "source"      json id
   rinf <- stringOption  "requestinfo" json id
   seed <- stringOptionM "randomseed"  json (defaultSeed options) (return . readM)
   enc  <- stringOptionM "encoding"    json [] readEncoding
   sch  <- stringOptionM "logging"     json Nothing (maybe (fail "invalid logging scheme") (return . Just) . readSchema)
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

stringOption :: String -> JSON -> (String -> a) -> IO (Maybe a)
stringOption attr json f = stringOptionM attr json Nothing (return . Just . f)

stringOptionM :: String -> JSON -> a -> (String -> IO a) -> IO a
stringOptionM attr json a f =
   case lookupM attr json of
      Right (String s) -> f s
      Right _  -> fail $ "Invalid value for " ++ attr ++ " (expecting string)"
      Left _   -> return a

myHandler :: Options -> DomainReasoner -> Request -> RPCHandler
myHandler opt1 dr request fun json = do
   srv <- either fail return $ findService dr (newId fun)
   Some ex <- case exerciseId request of
                 Just a  -> either fail return $ findExercise dr a
                 Nothing -> return (Some emptyExercise)
   opt2 <- makeOptions dr request
   let options = opt1 <> opt2
   evalService ex options jsonEvaluator srv json

jsonEvaluator :: Evaluator a JSON JSON
jsonEvaluator = Evaluator jsonTypeDecoder jsonEncoder