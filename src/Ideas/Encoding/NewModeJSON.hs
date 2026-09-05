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

module Ideas.Encoding.NewModeJSON (processJSON) where

import Control.Applicative
import Control.Exception
import System.IO.Error
import Data.Either
import Data.Maybe
import Ideas.Common.Library hiding (exerciseId)
import qualified Ideas.Encoding.ModeJSON as Legacy
import Ideas.Encoding.NewDecoderJSON
import Ideas.Encoding.NewEncoderJSON
import Ideas.Encoding.Evaluator
import Ideas.Encoding.Options (Options, makeOptions, maxTime, cgiBin)
import Ideas.Encoding.Request
import Ideas.Service.DomainReasoner
import Ideas.Text.JSON
import Ideas.Utils.Prelude (timedSeconds)

processJSON :: Options -> DomainReasoner -> String -> IO (Request, String, String)
processJSON options dr txt = do
   json <- either fail return (parseJSON txt)
   let legacy = isRight $ lookupM "params" json
   if legacy then Legacy.processJSON' options dr json else do
      req  <- jsonRequest options json
      resp <- jsonRPC2 dr (maybe "result" show $ serviceId req) json $ \fun arg ->
                 maybe id timedSeconds (maxTime options) (toJSON <$> myHandler options dr req fun arg)
      --unless (responseError resp == Null) $ !!!!!!!!!!!!!! 
      --   changeLog (logRef options) (\r -> r {errormsg = show (responseError resp)})
      let f   = if compactOutput req then compactJSON else show
      return (req, f resp, "application/json")

jsonRPC2 :: DomainReasoner -> String -> JSON -> RPCHandler -> IO JSON
jsonRPC2 dr serviceName json rpc = do
   okResponse dr serviceName <$> rpc serviceName json
 `catch` handler
 where
   handler :: SomeException -> IO JSON
   handler e =
      let msg = maybe (show e) ioeGetErrorString (fromException e)
      in return $ errorResponse dr (toJSON msg)

okResponse :: DomainReasoner -> String -> JSON -> JSON
okResponse dr serviceName result = Object
   [ (serviceName,  result)
   , ("version", String (version dr))
   ]

errorResponse :: DomainReasoner -> JSON -> JSON
errorResponse dr msg = Object 
   [ ("error", msg)
   , ("version", String (version dr))
   ]

-- TODO: Clean-up code
extractExerciseId :: JSON -> Maybe Id
extractExerciseId json = f <$> (
   get "exerciseid" json <|>
   (get "state" json >>= get "exerciseid"))
 where
   f (String s) = newId s
   f _          = error "expecting an exercise id"

   get s = either (const Nothing) Just . lookupM s 

jsonRequest :: Options -> JSON -> IO Request
jsonRequest options json = do
   let exId = extractExerciseId json
   srv  <- stringOption  "service"     json newId
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
      Right _ -> fail $ "Invalid value for " ++ attr ++ " (expecting string)"
      Left _  -> return a

myHandler :: Options -> DomainReasoner -> Request -> String -> JSON -> IO JSONBuilder
myHandler opt1 dr request fun json = do
   srv <- either fail return $ findService dr (newId fun)
   Some ex <- case exerciseId request of
                 Just a  -> either fail return $ findExercise dr a
                 Nothing -> return (Some emptyExercise)
   opt2 <- makeOptions dr request
   let options = opt1 <> opt2
   evalService ex options jsonEvaluator srv json

jsonEvaluator :: Evaluator a JSON JSONBuilder
jsonEvaluator = Evaluator jsonTypeDecoder jsonEncoder