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
import Control.Monad.State (StateT, evalStateT, get, gets, put)
import Data.Char
import Ideas.Service.DomainReasoner
import Ideas.Service.Evaluator
import Ideas.Service.Request
import Ideas.Service.EncoderJSON
import Ideas.Service.State
import Ideas.Service.Types hiding (String)
import System.Random
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
addVersion version json =
   case json of
      Object xs -> Object (xs ++ [info])
      _         -> json
 where
   info = ("version", String version)

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
   runEval dr (evalService (jsonConverter ex) srv) arg

type EvalJSON = StateT (DomainReasoner, JSON) IO

runEval :: DomainReasoner -> EvalJSON a -> JSON -> IO a
runEval dr m json = evalStateT m (dr, json)

jsonConverter :: Exercise a -> Evaluator (Const a) EvalJSON JSON
jsonConverter ex = Evaluator (jsonEncoder ex) (jsonDecoder ex)

jsonDecoder :: Exercise a -> Decoder (Type a) EvalJSON
jsonDecoder ex = Decoder (decode ex)
 where
   reader :: Exercise a -> EvalJSON a
   reader ex = do
      json <- gets snd
      case json of
         String s -> either (fail . show) return (parser ex s)
         _        -> fail "Expecting a string when reading a term"

   decode :: Exercise a -> Type a t -> EvalJSON t
   decode ex serviceType =
      case serviceType of
         Tp.Tag s t
            | otherwise ->
                 decode ex t         
         Tp.Iso p t  -> liftM (from p) (decode ex t)
         Pair t1 t2 -> do
            a <- decode ex t1
            b <- decode ex t2 
            return (a, b)
        
         t1 :|: t2 ->
            liftM Left  (decode ex t1) `mplus`
            liftM Right (decode ex t2)
         Unit -> return ()
         Const ctp ->
            case ctp of
               State ->  useFirst $ \json -> do
                    (dr, old) <- get
                    put (dr, json)
                    a <- decodeState ex (reader ex)
                    put (dr, old)
                    return a
               Context  -> useFirst $ \json -> do
                           (dr, old) <- get
                           put (dr, json)
                           a <- reader ex
                           put (dr, old)
                           return (inContext ex a)
               Exercise -> 
                        do (dr, json) <- get
                           case json of
                              Array (String _:rest) -> put (dr, Array rest) >> return ex
                              _ -> return ex
               Environment -> useFirst decodeContext
               Location -> useFirst decodeLocation
               Int -> useFirst $ \json ->
                              case json of
                                 Number (I n) -> return (fromIntegral n)
                                 _        -> fail "not an integer"
               Tp.String -> useFirst $ \json -> 
                                 case json of
                                    String s -> return s
                                    _        -> fail "not a string"
               Rule     -> useFirst $ \x -> jsonToId x >>= getRule ex
               StdGen   -> liftIO newStdGen
               Script   -> do dr <- gets fst
                              lift (defaultScript dr (getId ex))
               _        -> fail $ "No support for argument type: " ++ show serviceType

   useFirst :: (JSON -> EvalJSON a) -> EvalJSON a
   useFirst f = do
      (dr, json) <- get
      case json of
         Array (x:xs) -> do
            a <- f x
            put (dr, Array xs)
            return a
         _ -> fail "expecting an argument"

jsonToId :: Monad m => JSON -> m Id
jsonToId = liftM (newId :: String -> Id) . fromJSON

decodeLocation :: Monad m => JSON -> m Location
decodeLocation (String s) = liftM toLocation (readM s)
decodeLocation _          = fail "expecting a string for a location"

--------------------------

decodeState :: Exercise a -> EvalJSON a -> EvalJSON (State a)
decodeState ex f = do
   json <- gets snd
   rec json
 where
   deintercalate xs =  
       let (ys, zs) = break (==';') xs 
       in  ys : case zs of []      -> []
                           (_:zs') -> deintercalate zs'
   rec (Array [a]) = rec a
   rec (Array [String _code, String p, ce, jsonContext]) = do
      ps   <- mapM (readM >>= liftM (`makePrefix` strategy ex)) $ deintercalate p
      a    <- do (dr, old) <- get 
                 put (dr, ce)
                 a <- f
                 put (dr, old)
                 return a
      env  <- decodeContext jsonContext
      return $ makeState ex ps (makeContext ex env a)
   rec s = fail $ "invalid state" ++ show s

decodeContext :: Monad m => JSON -> m Environment
decodeContext (String "") = decodeContext (Object []) -- Being backwards compatible (for now)
decodeContext (Object xs) = foldM (flip add) mempty xs
 where
   add :: Monad m => (String, JSON) -> Environment -> m Environment
   add (k, String s) = return . insertRef (makeRef k) s
   add _             = fail "invalid item in context"
decodeContext json = fail $ "invalid context: " ++ show json