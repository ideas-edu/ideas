-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
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
module Service.ModeJSON (processJSON, jsonTuple) where

import Common.Context
import Common.Utils (Some(..), distinct, readM)
import Common.Exercise
import Common.Strategy (makePrefix)
import Common.Transformation hiding (ruleList, defaultArgument)
import Text.JSON
import Service.ExerciseList
import Service.Request
import Service.Types (TypedValue(..), Evaluator(..), Type, encodeDefault, decodeDefault, Encoder(..), Decoder(..), decoderExercise)
import qualified Service.Types as Tp
import qualified Service.TypedAbstractService as TAS
import Service.ServiceList hiding (Service)
import qualified Service.ExerciseList as List
import Control.Monad
import Data.Maybe
import Data.Char

-- TODO: Clean-up code
extractCode :: JSON -> ExerciseCode
extractCode = fromMaybe noCode . readCode . f
 where 
   f (String s) = s
   f (Array [String _, String _, a@(Array _)]) = f a
   f (Array (String s:tl)) | any (\c -> not (isAlphaNum c || isSpace c || c `elem` ".-")) s = f (Array tl)
   f (Array (hd:_)) = f hd
   f _ = ""
      
processJSON :: String -> IO (Request, String, String)
processJSON input = do
   txt <- jsonRPC input myHandler
   case fmap jsonRequest (parseJSON input) of
      Just (Right req) -> return (req, txt, "application/json")
      Just (Left err)  -> fail err
      _                -> fail "no parse"

jsonRequest :: JSON -> Either String Request
jsonRequest json = do
   srv  <- case lookupM "method" json of
              Just (String s) -> return s
              _               -> fail "Invalid method"
   code <- liftM (return . extractCode) (lookupM "params" json)
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
      , exerciseID = code
      , source     = src
      , dataformat = JSON
      , encoding   = enc
      }

myHandler :: JSON_RPC_Handler
myHandler fun arg 
--   | fun == "ping" =
--        return (String "ok")
   | code == noCode && fun /= "exerciselist" =
        fail "invalid exercise code"
   | otherwise = 
        case jsonConverter code of
           Some conv -> do
              service <- getService fun
              either fail return (evalService conv service arg)
 where 
   code = extractCode arg

jsonConverter :: ExerciseCode -> Some (Evaluator (Either String) JSON JSON)
jsonConverter code =
   let f a = Some (Evaluator (jsonEncoder (List.exercise a)) (jsonDecoder a))
   in case List.getPackage code of
         Just (Some pkg) -> f pkg
         Nothing         -> f (package emptyExercise)

jsonEncoder :: Monad m => Exercise a -> Encoder m JSON a
jsonEncoder ex = Encoder
   { encodeType  = encode (jsonEncoder ex)
   , encodeTerm  = return . String . prettyPrinter ex
   , encodeTuple = jsonTuple
   }
 where
   encode :: Monad m => Encoder m JSON a -> Type a t -> t -> m JSON
   encode enc serviceType a
      | length xs > 1 =
           liftM jsonTuple (mapM (\(b ::: t) -> encode enc t b) xs)
      | otherwise = 
           case serviceType of
              Tp.List t    -> liftM Array (mapM (encode enc t) a)
              Tp.Tag s t   -> liftM (\b -> Object [(s, b)]) (encode enc t a)
              Tp.Int       -> return (toJSON a)
              Tp.Bool      -> return (toJSON a)
              Tp.String    -> return (toJSON a)
              Tp.State     -> encodeState (encodeTerm enc) a
              Tp.Result    -> encodeResult enc a
              _            -> encodeDefault enc serviceType a
    where
      xs = tupleList (a ::: serviceType)
    
   tupleList :: TypedValue a -> [TypedValue a]
   tupleList (a ::: Tp.Iso _ f t)   = tupleList (f a ::: t)
   tupleList (p ::: Tp.Pair t1 t2) = 
      tupleList (fst p ::: t1) ++ tupleList (snd p ::: t2)
   tupleList tv = [tv]
         
jsonDecoder :: MonadPlus m => ExercisePackage a -> Decoder m JSON a
jsonDecoder pkg = Decoder
   { decodeType     = decode (jsonDecoder pkg)
   , decodeTerm     = reader (exercise pkg)
   , decoderPackage = pkg
   }
 where
   reader :: Monad m => Exercise a -> JSON -> m a
   reader ex (String s) = either (fail . show) return (parser ex s)
   reader _  _          = fail "Expecting a string when reading a term"
 
   decode :: MonadPlus m => Decoder m JSON a -> Type a t -> JSON -> m (t, JSON) 
   decode dec serviceType =
      case serviceType of
         Tp.State    -> useFirst $ decodeState (decoderExercise dec) (decodeTerm dec)
         Tp.Location -> useFirst fromJSON
         Tp.Term     -> useFirst $ decodeTerm dec
         Tp.Rule     -> useFirst $ \x -> fromJSON x >>= getRule (decoderExercise dec)
         Tp.Exercise -> \json -> case json of
                                    (Array (String _:rest)) -> return (decoderExercise dec, Array rest)
                                    _ -> return (decoderExercise dec, json)
         Tp.Int      -> useFirst $ \json -> case json of 
                                               Number (I n) -> return (fromIntegral n)
                                               _        -> fail "not an integer"
         Tp.String   -> useFirst $ \json -> case json of 
                                               String s -> return s
                                               _        -> fail "not a string"
         _           -> decodeDefault dec serviceType
   
   useFirst :: Monad m => (JSON -> m a) -> JSON -> m (a, JSON)
   useFirst f (Array (x:xs)) = do
      a <- f x
      return (a, Array xs)
   useFirst _ _ = fail "expecting an argument"
         
instance InJSON Location where
   toJSON              = toJSON . show
   fromJSON (String s) = readM s
   fromJSON _          = fail "expecting a string"

--------------------------

encodeState :: Monad m => (a -> m JSON) -> TAS.State a -> m JSON
encodeState f st = do 
   theTerm <- f (TAS.term st)
   return $ Array
      [ String (show (exerciseCode (TAS.exercise st)))
      , String (maybe "NoPrefix" show (TAS.prefix st))
      , theTerm
      , encodeContext (getEnvironment (TAS.context st))
      ]

encodeContext :: Environment -> JSON
encodeContext env = Object (map f (keysEnv env))
 where
   f k = (k, String $ fromMaybe "" $ lookupEnv k env)

decodeState :: Monad m => Exercise a -> (JSON -> m a) -> JSON -> m (TAS.State a)
decodeState ex f (Array [a]) = decodeState ex f a
decodeState ex f (Array [String _code, String p, ce, jsonContext]) = do
   a    <- f ce 
   env  <- decodeContext jsonContext
   return TAS.State 
      { TAS.exercise = ex
      , TAS.prefix   = readM p >>= (`makePrefix` strategy ex)
      , TAS.context  = makeContext env a
      }
decodeState _ _ s = fail $ "invalid state" ++ show s

decodeContext :: Monad m => JSON -> m Environment
decodeContext (String "") = decodeContext (Object []) -- Being backwards compatible (for now)
decodeContext (Object xs) = foldM add emptyEnv xs
 where 
   add env (k, String s) = return (storeEnv k s env)       
   add _ _ = fail "invalid item in context"
decodeContext json = fail $ "invalid context: " ++ show json
   
encodeResult :: Monad m => Encoder m JSON a -> TAS.Result a -> m JSON
encodeResult enc result =
   case result of
      -- TAS.SyntaxError _ -> [("result", String "SyntaxError")]
      TAS.Buggy rs      -> return $ Object [("result", String "Buggy"), ("rules", Array $ map (String . name) rs)]
      TAS.NotEquivalent -> return $ Object [("result", String "NotEquivalent")]   
      TAS.Ok rs st      -> do
         json <- encodeType enc Tp.State st
         return $ Object [("result", String "Ok"), ("rules", Array $ map (String . name) rs), ("state", json)]
      TAS.Detour rs st  -> do
         json <- encodeType enc Tp.State st
         return $ Object [("result", String "Detour"), ("rules", Array $ map (String . name) rs), ("state", json)]
      TAS.Unknown st    -> do
         json <- encodeType enc Tp.State st
         return $ Object [("result", String "Unknown"), ("state", json)]

jsonTuple :: [JSON] -> JSON
jsonTuple xs = 
   case mapM f xs of 
      Just xs | distinct (map fst xs) -> Object xs
      _ -> Array xs
 where
   f (Object [p]) = Just p
   f _ = Nothing