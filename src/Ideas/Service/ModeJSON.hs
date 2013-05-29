{-# LANGUAGE GADTs #-}
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
module Ideas.Service.ModeJSON (processJSON) where

import Ideas.Common.Library hiding (exerciseId)
import Ideas.Common.Utils (Some(..), readM)
import Control.Monad
import Data.Char
import Ideas.Service.DomainReasoner
import Ideas.Service.Evaluator
import Ideas.Service.Request
import Ideas.Service.EncoderJSON
import Ideas.Service.State
import Ideas.Service.FeedbackScript.Syntax (Script)
import Ideas.Service.Types hiding (String)
import System.Random hiding (getStdGen)
import Ideas.Text.JSON
import qualified Ideas.Service.Types as Tp

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

processJSON :: DomainReasoner -> String -> IO (Request, String, String)
processJSON dr input = do
   json <- either fail return (parseJSON input)
   req  <- jsonRequest json
   resp <- jsonRPC json (myHandler dr)
   let out = show $ addVersion (version dr) (toJSON resp)
   return (req, out, "application/json")

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
myHandler dr fun arg = do
   srv <- findService dr (newId fun)
   Some ex <- 
      if fun == "exerciselist"
      then return (Some emptyExercise)
      else extractExerciseId arg >>= findExercise dr
   script <- defaultScript dr (getId ex)
   stdgen <- newStdGen
   let jds = JSONDecoderState ex script stdgen
   runEncoderStateM (evalService (jsonConverter ex) srv) jds arg

type JSONDecoder a = EncoderState (JSONDecoderState a) JSON

data JSONDecoderState a = JSONDecoderState
   { getExercise :: Exercise a
   , getScript   :: Script
   , getStdGen   :: StdGen
   }

jsonConverter :: Exercise a -> Evaluator a (JSONDecoder a) JSON
jsonConverter ex = Evaluator
   (runEncoderStateM jsonEncoder (String . prettyPrinter ex)) 
   jsonDecoder

jsonDecoder :: Type a t -> JSONDecoder a t
jsonDecoder tp = encoderFor $ \json ->
   case json of
      Array xs -> liftM fst (decodeType tp // xs)
      _ -> fail "expecting an array"

decodeType :: Type a t -> EncoderState (JSONDecoderState a) [JSON] (t, [JSON])
decodeType tp =
   case tp of
      Tp.Tag _ t -> decodeType t         
      Tp.Iso p t -> change (from p) (decodeType t)
      Pair t1 t2 -> do
         (a, xs) <- decodeType t1
         (b, ys) <- decodeType t2 // xs
         return ((a, b), ys)
      t1 :|: t2 ->
         change Left  (decodeType t1) `mplus`
         change Right (decodeType t2)
      Unit         -> result ()
      Const StdGen -> withState getStdGen >>= result
      Const Script -> withState getScript >>= result
      Const t      -> encoderFor $ \xs -> 
                         case xs of
                            hd:tl -> do a <- decodeConst t // hd
                                        return (a, tl)
                            _     -> fail "no more elements"
      _ -> fail $ "No support for argument type: " ++ show tp
 where
   result a = simpleEncoder (\xs -> (a, xs))
   change f = liftM (first f)

decodeConst :: Const a t -> JSONDecoder a t
decodeConst tp =
   case tp of
      State       -> decodeState
      Context     -> decodeContext
      Exercise    -> withState getExercise
      Environment -> decodeEnvironment
      Location    -> decodeLocation
      Int         -> maybeEncoder fromJSON
      Tp.String   -> maybeEncoder fromJSON
      Rule        -> decodeRule
      _           -> fail $ "No support for argument type: " ++ show tp

decodeRule :: JSONDecoder a (Rule (Context a))
decodeRule = do
   ex <- withState getExercise 
   encoderFor $ \json -> 
      case json of
         String s -> getRule ex (newId s)
         _        -> fail "expecting a string for rule"

decodeLocation :: JSONDecoder a Location
decodeLocation = encoderFor $ \json -> 
   case json of
      String s -> liftM toLocation (readM s)
      _        -> fail "expecting a string for a location"

decodeState :: JSONDecoder a (State a)
decodeState = do
   ex <- withState getExercise 
   encoderFor $ \json ->
      case json of
         Array [a] -> decodeState // a
         Array [String _code, pref, term, jsonContext] -> do
            ps   <- decodePrefixes    // pref
            a    <- decodeTerm        // term
            env  <- decodeEnvironment // jsonContext
            return $ makeState ex ps (makeContext ex env a)
         _ -> fail $ "invalid state" ++ show json
 
decodePrefixes :: JSONDecoder a [Prefix (Context a)]
decodePrefixes = do
   ex <- withState getExercise
   encoderFor $ \json -> 
      case json of
         String p -> forM (deintercalate p) $ do
                        (readM >>= liftM (`makePrefix` strategy ex))
         _ -> fail "invalid prefixes"

decodeEnvironment :: JSONDecoder a Environment
decodeEnvironment = encoderFor $ \json -> 
   case json of 
      String "" -> decodeEnvironment // Object []
      Object xs -> foldM (flip add) mempty xs
      _         -> fail $ "invalid context: " ++ show json
 where
   add (k, String s) = return . insertRef (makeRef k) s
   add _             = fail "invalid item in context"

decodeContext :: JSONDecoder a (Context a) 
decodeContext = do
   ex <- withState getExercise
   liftM (inContext ex) decodeTerm

decodeTerm :: JSONDecoder a a
decodeTerm = do
   ex <- withState getExercise
   eitherEncoder $ \json ->
      case json of
         String s -> parser ex s
         _        -> Left $ "Expecting a string when reading a term"

-- local helper
deintercalate :: String -> [String]
deintercalate xs
   | null zs   = [ys]
   | otherwise = ys : deintercalate (drop 1 zs)
 where
   (ys, zs) = break (== ';') xs