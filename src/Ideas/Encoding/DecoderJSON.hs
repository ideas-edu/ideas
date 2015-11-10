{-# LANGUAGE GADTs #-}
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

module Ideas.Encoding.DecoderJSON
   ( JSONDecoder, jsonDecoder
   ) where

import Control.Monad
import Data.Maybe
import Ideas.Common.Library hiding (exerciseId, symbol)
import Ideas.Common.Traversal.Navigator
import Ideas.Encoding.Encoder
import Ideas.Service.State
import Ideas.Service.Types hiding (String)
import Ideas.Text.JSON
import qualified Ideas.Service.Types as Tp

type JSONDecoder a = Decoder a JSON

jsonDecoder :: TypedDecoder a JSON
jsonDecoder tp = decoderFor $ \json ->
   case json of
      Array xs -> decodeType tp // xs
      _ -> fail "expecting an array"

decodeType :: Type a t -> Decoder a [JSON] t
decodeType tp =
   case tp of
      Tag _ t -> decodeType t
      Iso p t -> liftM (from p) (decodeType t)
      Pair t1 t2 -> do
         a <- decodeType t1
         b <- decodeType t2
         return (a, b)
      t1 :|: t2 ->
         liftM Left  (decodeType t1) `mplus`
         liftM Right (decodeType t2)
      Unit         -> return ()
      Const StdGen -> getStdGen
      Const Script -> getScript
      Const t      -> symbol >>= \a -> decodeConst t // a
      _ -> fail $ "No support for argument type: " ++ show tp

decodeConst :: Const a t -> JSONDecoder a t
decodeConst tp =
   case tp of
      State       -> decodeState
      Context     -> decodeContext
      Exercise    -> getExercise
      Environment -> decodeEnvironment
      Location    -> decodeLocation
      Int         -> decoderFor fromJSON
      Tp.String   -> decoderFor fromJSON
      Id          -> decodeId
      Rule        -> decodeRule
      _           -> fail $ "No support for argument type: " ++ show tp

decodeRule :: JSONDecoder a (Rule (Context a))
decodeRule = do
   ex <- getExercise
   decoderFor $ \json ->
      case json of
         String s -> getRule ex (newId s)
         _        -> fail "expecting a string for rule"

decodeId :: JSONDecoder a Id
decodeId = decoderFor $ \json ->
   case json of
      String s -> return (newId s)
      _        -> fail "expecting a string for id"

decodeLocation :: JSONDecoder a Location
decodeLocation = decoderFor $ \json ->
   case json of
      String s -> liftM toLocation (readM s)
      _        -> fail "expecting a string for a location"

decodeState :: JSONDecoder a (State a)
decodeState = do
   ex <- getExercise
   decoderFor $ \json ->
      case json of
         Array [a] -> setInput a >> decodeState
         Array (String _code : pref : term : jsonContext : rest) -> do
            pts  <- decodePaths       // pref
            a    <- decodeTerm        // term
            env  <- decodeEnvironment // jsonContext
            let loc = envToLoc env
                ctx = navigateTowards loc $ deleteRef locRef $
                         setEnvironment env $ inContext ex a
                prfx = replayPaths pts (strategy ex) ctx
            case rest of
               [] -> return $ makeState ex prfx ctx
               [Array [String user, String session, String startterm]] ->
                  return (makeState ex prfx ctx)
                     { stateUser      = Just user
                     , stateSession   = Just session
                     , stateStartTerm = Just startterm
                     }
               _  -> fail $ "invalid state" ++ show json
         _ -> fail $ "invalid state" ++ show json

envToLoc :: Environment -> Location
envToLoc env = toLocation $ fromMaybe [] $ locRef ? env >>= readM

locRef :: Ref String
locRef = makeRef "location"

decodePaths :: JSONDecoder a [Path]
decodePaths =
   decoderFor $ \json ->
      case json of
         String p -> readPaths p
         _ -> fail "invalid prefixes"

decodeEnvironment :: JSONDecoder a Environment
decodeEnvironment = decoderFor $ \json ->
   case json of
      String "" -> return mempty
      Object xs -> foldM (flip add) mempty xs
      _         -> fail $ "invalid context: " ++ show json
 where
   add (k, String s) = return . insertRef (makeRef k) s
   add _             = fail "invalid item in context"

decodeContext :: JSONDecoder a (Context a)
decodeContext = do
   ex <- getExercise
   liftM (inContext ex) decodeTerm

decodeTerm :: JSONDecoder a a
decodeTerm = do
   ex <- getExercise
   decoderFor $ \json ->
      case json of
         String s -> either fail return (parser ex s)
         _        -> fail "Expecting a string when reading a term"