{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- Copyright 2013, Open Universiteit Nederland. This file is distributed
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
module Ideas.Encoding.DecoderJSON
   ( JSONDecoder, JSONDecoderState(..), jsonDecoder
   ) where

import Control.Monad
import Ideas.Common.Library hiding (exerciseId)
import Ideas.Encoding.Evaluator
import Ideas.Service.FeedbackScript.Syntax (Script)
import Ideas.Service.State
import Ideas.Service.Types hiding (String)
import Ideas.Text.JSON
import System.Random hiding (getStdGen)
import qualified Ideas.Service.Types as Tp

type JSONDecoder a = EncoderState (JSONDecoderState a) JSON

data JSONDecoderState a = JSONDecoderState
   { getExercise :: Exercise a
   , getScript   :: Script
   , getStdGen   :: StdGen
   }

jsonDecoder :: Type a t -> JSONDecoder a t
jsonDecoder tp = encoderFor $ \json ->
   case json of
      Array xs -> liftM fst (decodeType tp // xs)
      _ -> fail "expecting an array"

decodeType :: Type a t -> EncoderState (JSONDecoderState a) [JSON] (t, [JSON])
decodeType tp =
   case tp of
      Tag _ t -> decodeType t
      Iso p t -> change (from p) (decodeType t)
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
      Id          -> decodeId
      Rule        -> decodeRule
      _           -> fail $ "No support for argument type: " ++ show tp

decodeRule :: JSONDecoder a (Rule (Context a))
decodeRule = do
   ex <- withState getExercise
   encoderFor $ \json ->
      case json of
         String s -> getRule ex (newId s)
         _        -> fail "expecting a string for rule"

decodeId :: JSONDecoder a Id
decodeId = encoderFor $ \json ->
   case json of
      String s -> return (newId s)
      _        -> fail "expecting a string for id"

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
            iss  <- decodePrefixes    // pref
            a    <- decodeTerm        // term
            env  <- decodeEnvironment // jsonContext
            let ctx = setEnvironment env (inContext ex a)
            ps   <- mapM (\is -> makePrefix is (strategy ex) ctx) iss
            return $ makeState ex ps ctx
         _ -> fail $ "invalid state" ++ show json

decodePrefixes :: JSONDecoder a [Path]
decodePrefixes = do
   encoderFor $ \json ->
      case json of
         String p -> mapM readM (deintercalate p)
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
         _        -> Left "Expecting a string when reading a term"

-- local helper
deintercalate :: String -> [String]
deintercalate xs
   | null zs   = [ys]
   | otherwise = ys : deintercalate (drop 1 zs)
 where
   (ys, zs) = break (== ';') xs