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
import Control.Monad
import System.IO.Error
import Data.Maybe
import Data.Semigroup ((<>))
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
   let legacy = isJust $ lookupM "params" json
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
extractExerciseId :: (MonadPlus m, MonadFail m) => JSON -> m Id
extractExerciseId json = f <$> (
   lookupM "exerciseid" json <|>
   (lookupM "state" json >>= lookupM "exerciseid"))
 where
   f (String s) = newId s
   f _          = error "expecting an exercise id"

jsonRequest :: MonadFail m => Options -> JSON -> m Request
jsonRequest options json = do
   let exId = extractExerciseId json
   srv  <- stringOption  "service"     json newId
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

stringOption :: MonadFail m => String -> JSON -> (String -> a) -> m (Maybe a)
stringOption attr json f = stringOptionM attr json Nothing (return . Just . f)

stringOptionM :: MonadFail m => String -> JSON -> a -> (String -> m a) -> m a
stringOptionM attr json a f =
   case lookupM attr json of
      Just (String s) -> f s
      Just _  -> fail $ "Invalid value for " ++ attr ++ " (expecting string)"
      Nothing -> return a

myHandler :: Options -> DomainReasoner -> Request -> String -> JSON -> IO JSONBuilder
myHandler opt1 dr request fun json = do
   srv <- findServiceM dr (newId fun)
   Some ex <- case exerciseId request of
                 Just a  -> findExercise dr a
                 Nothing -> return (Some emptyExercise)
   opt2 <- makeOptions dr request
   let options = opt1 <> opt2
   evalService ex options jsonEvaluator srv json

jsonEvaluator :: Evaluator a JSON JSONBuilder
jsonEvaluator = Evaluator jsonTypeDecoder jsonEncoder