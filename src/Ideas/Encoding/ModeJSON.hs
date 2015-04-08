-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Services using JSON notation
--
-----------------------------------------------------------------------------
--  $Id$

module Ideas.Encoding.ModeJSON (processJSON) where

import Data.Char
import Ideas.Common.Library hiding (exerciseId)
import Ideas.Common.Utils (Some(..), timedSeconds)
import Ideas.Encoding.DecoderJSON
import Ideas.Encoding.Encoder (makeOptions)
import Ideas.Encoding.EncoderJSON
import Ideas.Encoding.Evaluator
import Ideas.Service.DomainReasoner
import Ideas.Service.Request
import Ideas.Text.JSON

processJSON :: Maybe Int -> Maybe String -> DomainReasoner -> String -> IO (Request, String, String)
processJSON maxTime cgiBin dr input = do
   json <- either fail return (parseJSON input)
   req  <- jsonRequest cgiBin json
   resp <- jsonRPC json $ \fun arg ->
              maybe id timedSeconds maxTime (myHandler dr req fun arg)
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
   srv  <- case lookupM "method" json of
              Just (String s) -> return (Just (newId s))
              Nothing         -> return Nothing
              _               -> fail "Invalid method"
   let exId = lookupM "params" json >>= extractExerciseId
   enc  <- case lookupM "encoding" json of
              Nothing         -> return []
              Just (String s) -> readEncoding s
              _               -> fail "Invalid encoding"
   src  <- case lookupM "source" json of
              Nothing         -> return Nothing
              Just (String s) -> return (Just s)
              _               -> fail "Invalid source"
   let uid = case lookupM "id" json of
                Just (String s)     -> Just s
                Just (Number (I n)) -> Just (show n)
                _                   -> Nothing
   return emptyRequest
      { serviceId  = srv
      , exerciseId = exId
      , user       = uid
      , source     = src
      , cgiBinary  = cgiBin
      , dataformat = JSON
      , encoding   = enc
      }

myHandler :: DomainReasoner -> Request -> RPCHandler
myHandler dr request fun json = do
   srv <- findService dr (newId fun)
   Some options <- makeOptions dr request
   evalService options jsonEvaluator srv json

jsonEvaluator :: Evaluator a JSON JSON
jsonEvaluator = Evaluator jsonDecoder jsonEncoder