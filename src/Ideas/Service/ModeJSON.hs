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
import Control.Monad.Error
import Control.Monad.State (StateT, evalStateT, get, put)
import Data.Char
import Ideas.Service.DomainReasoner
import Ideas.Service.Evaluator
import Ideas.Service.Request
import Ideas.Service.EncoderJSON
import Ideas.Service.State
import Ideas.Service.FeedbackScript.Syntax (Script)
import Ideas.Service.Types hiding (String)
import System.Random
import Ideas.Text.JSON
import qualified Ideas.Service.Types as Tp
import System.IO.Unsafe (unsafePerformIO)

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
   Some ex <- 
      if fun == "exerciselist"
      then return (Some emptyExercise)
      else extractExerciseId arg >>= findExercise dr
   srv <- findService dr (newId fun)
   runEval (evalService (jsonConverter dr ex) srv) arg

type EvalJSON = StateT JSON (EncoderState () ())

data JSONDecoderState a = JSONDecoderState
   { getDomainReasoner :: DomainReasoner
   , getExercise       :: Exercise a
   , getScript         :: Script
   , getStdGen         :: StdGen
   }

runEval :: EvalJSON a -> JSON -> IO a
runEval m json = runEncoderStateM (evalStateT m json) () ()

jsonConverter :: DomainReasoner -> Exercise a -> Evaluator (Const a) EvalJSON JSON
jsonConverter dr ex = Evaluator 
   (runEncoderStateM jsonEncoder (String . prettyPrinter ex)) 
   (\tp -> do xs <- get; (a, ys) <- runEncoderStateM (jsonDecoder dr ex tp) () xs; put (Array ys); return a) 

jsonDecoder :: DomainReasoner -> Exercise a -> Type a t -> EncoderState () JSON (t, [JSON])
jsonDecoder dr ex tp = encoderFor $ \json ->
   case json of
      Array xs -> do (a, rest) <- decode dr ex tp // xs
                     return (a, rest)
      _ -> fail "expecting an array"

decode :: DomainReasoner -> Exercise a -> Type a t -> EncoderState () [JSON] (t, [JSON])
decode dr ex tp =
   case tp of
      Tp.Tag _ t -> decode dr ex t         
      Tp.Iso p t -> change (from p) (decode dr ex t)
      Pair t1 t2 -> do
         (a, xs) <- decode dr ex t1
         (b, ys) <- decode dr ex t2 // xs
         return ((a, b), ys)
      t1 :|: t2 ->
         change Left  (decode dr ex t1) `mplus`
         change Right (decode dr ex t2)
      Unit         -> result ()
      Const StdGen -> result $ unsafePerformIO newStdGen
      Const Script -> result $ unsafePerformIO (defaultScript dr (getId ex))
      Const t      -> encoderFor $ \xs -> 
                         case xs of
                            hd:tl -> do a <- decodeConst ex t // hd
                                        return (a, tl)
                            _     -> fail "no more elements"
      _ -> fail $ "No support for argument type: " ++ show tp
 where
   result a = simpleEncoder (\xs -> (a, xs))
   change f = liftM (first f)

decodeConst :: Exercise a -> Const a t -> EncoderState () JSON t
decodeConst ex tp =
   case tp of
      State       -> decodeState ex
      Context     -> decodeContext ex
      Exercise    -> return ex
      Environment -> decodeEnvironment
      Location    -> decodeLocation
      Int         -> maybeEncoder fromJSON
      Tp.String   -> maybeEncoder fromJSON
      Rule        -> decodeRule ex
      _           -> fail $ "No support for argument type: " ++ show tp

decodeRule :: Exercise a -> EncoderState () JSON (Rule (Context a))
decodeRule ex = encoderFor $ \json -> 
   case json of
      String s -> getRule ex (newId s)
      _        -> fail "expecting a string for rule"

decodeLocation :: EncoderState () JSON Location
decodeLocation = encoderFor $ \json -> 
   case json of
      String s -> liftM toLocation (readM s)
      _        -> fail "expecting a string for a location"

decodeState :: Exercise a -> EncoderState () JSON (State a)
decodeState ex = encoderFor $ \json ->
   case json of
      Array [a] -> decodeState ex // a
      Array [String _code, pref, term, jsonContext] -> do
         ps   <- decodePrefixes ex // pref
         a    <- decodeTerm ex     // term
         env  <- decodeEnvironment // jsonContext
         return $ makeState ex ps (makeContext ex env a)
      _ -> fail $ "invalid state" ++ show json
 
decodePrefixes :: Exercise a -> EncoderState () JSON [Prefix (Context a)]
decodePrefixes ex = encoderFor $ \json -> 
   case json of
      String p -> forM (deintercalate p) $ do
                     (readM >>= liftM (`makePrefix` strategy ex))
      _ -> fail "invalid prefixes"

decodeEnvironment :: EncoderState () JSON Environment
decodeEnvironment = encoderFor $ \json -> 
   case json of 
      String "" -> decodeEnvironment // Object []
      Object xs -> foldM (flip add) mempty xs
      _         -> fail $ "invalid context: " ++ show json
 where
   add (k, String s) = return . insertRef (makeRef k) s
   add _             = fail "invalid item in context"

decodeContext :: Exercise a -> EncoderState () JSON (Context a) 
decodeContext ex = liftM (inContext ex) (decodeTerm ex)

decodeTerm :: Exercise a -> EncoderState () JSON a
decodeTerm ex = eitherEncoder $ \json ->
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