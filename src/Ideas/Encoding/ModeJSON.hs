-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
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
module Ideas.Encoding.ModeJSON (processJSON) where

import Ideas.Common.Library hiding (exerciseId)
import Ideas.Common.Utils (Some(..), timedSeconds)
import Control.Monad
import Data.Char
import Ideas.Service.DomainReasoner
import Ideas.Encoding.Evaluator
import Ideas.Service.Request
import Ideas.Encoding.DecoderJSON 
import Ideas.Encoding.EncoderJSON
import System.Random hiding (getStdGen)
import Ideas.Text.JSON

processJSON :: Bool -> DomainReasoner -> String -> IO (Request, String, String)
processJSON cgiMode dr input = do
   json <- either fail return (parseJSON input)
   req  <- jsonRequest json
   resp <- jsonRPC json (myHandler dr)
   let f   = if cgiMode then showCompact else showPretty
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

jsonRequest :: Monad m => JSON -> m Request
jsonRequest json = do
   srv  <- case lookupM "method" json of
              Just (String s) -> return s
              _               -> fail "Invalid method"
   let a = lookupM "params" json >>= extractExerciseId
   enc  <- case lookupM "encoding" json of
              Nothing         -> return Nothing
              Just (String s) -> liftM Just (readEncoding s)
              _               -> fail "Invalid encoding"
   src  <- case lookupM "source" json of
              Nothing         -> return Nothing
              Just (String s) -> return (Just s)
              _               -> fail "Invalid source"
   return Request
      { service    = srv
      , exerciseId = a
      , source     = src
      , dataformat = JSON
      , encoding   = enc
      }

myHandler :: DomainReasoner -> JSON_RPC_Handler IO
myHandler dr fun arg = timedSeconds 5 $ do
   srv <- findService dr (newId fun)
   Some ex <- 
      if fun == "exerciselist"
      then return (Some emptyExercise)
      else extractExerciseId arg >>= findExercise dr
   script <- defaultScript dr (getId ex)
   stdgen <- newStdGen
   let jds = JSONDecoderState ex script stdgen
   runEncoderStateM (evalService (jsonConverter ex) srv) jds arg

jsonConverter :: Exercise a -> Evaluator a (JSONDecoder a) JSON
jsonConverter ex = Evaluator
   (runEncoderStateM jsonEncoder (String . prettyPrinter ex)) 
   jsonDecoder