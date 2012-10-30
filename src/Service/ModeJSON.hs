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
module Service.ModeJSON (processJSON, jsonTuple) where

import Common.Library hiding (exerciseId)
import Common.Utils (Some(..), distinct, readM)
import Control.Monad.Error
import Control.Monad.State (StateT, evalStateT, get, put)
import Data.Char
import qualified Data.Map as M
import Service.DomainReasoner
import Service.Evaluator
import Service.Request
import Service.State
import Service.Submit
import Service.Types hiding (String)
import System.Random
import Text.JSON
import qualified Service.Types as Tp

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

processJSON :: String -> DomainReasoner (Request, String, String)
processJSON input = do
   json <- either throwError return (parseJSON input)
   req  <- jsonRequest json
   vers <- getVersion
   resp <- jsonRPC json myHandler
   let out = show $ (if null vers then id else addVersion vers) (toJSON resp)
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

myHandler :: JSON_RPC_Handler DomainReasoner
myHandler fun arg = do
   Some ex <- 
      if fun == "exerciselist"
      then return (Some emptyExercise)
      else extractExerciseId arg >>= findExercise
   srv <- findService fun
   runEval (evalService (jsonConverter ex) srv) arg

type EvalJSON = StateT JSON DomainReasoner

runEval :: EvalJSON a -> JSON -> DomainReasoner a
runEval = evalStateT

jsonConverter :: Exercise a -> Evaluator (Const a) EvalJSON JSON
jsonConverter ex = Evaluator (jsonEncoder ex) (jsonDecoder ex)

jsonEncoder :: Exercise a -> Encoder (Type a) EvalJSON JSON
jsonEncoder ex = jsonEncode (String . prettyPrinter ex)

jsonEncode :: (a -> JSON) -> Encoder (Type a) EvalJSON JSON
jsonEncode enc tv@(val ::: tp)
   | length (tupleList tv) > 1 =
        liftM jsonTuple (mapM (jsonEncode enc) (tupleList tv))
   | otherwise =
        -- encodeWith (jsonEncoderMap enc) (jsonEncodeConst enc) tv
        case tp of
           Iso p t   -> jsonEncode enc (to p val ::: t)
           t1 :|: t2 -> case val of
              Left  x -> jsonEncode enc (x ::: t1)
              Right y -> jsonEncode enc (y ::: t2)
           
           Pair t1 t2 -> do
              x <- jsonEncode enc (fst val ::: t1)
              y <- jsonEncode enc (snd val ::: t2)
              return (jsonTuple [x, y])
           Tp.Tag s t
              | s `elem` ["elem", "list"] ->
                   jsonEncode enc (val ::: t)
              | s == "Result" -> do
                   conv <- equalM tp submitType
                   encodeResult enc (conv val)
              | s == "state" -> do
                   conv <- equalM tp stateType
                   return (encodeState enc (conv val))
              | s == "Exception" -> do
                   f <- equalM t stringType
                   fail (f val)
              | s == "Location" -> do
                   f <- equalM t (List intType)
                   return (toJSON (show (f val)))
              | otherwise -> liftM (\b -> Object [(s, b)]) (jsonEncode enc (val ::: t))

           Tp.Unit   -> return Null
           Tp.List t -> liftM Array (mapM (jsonEncode enc . (::: t)) val)
           Const ctp -> jsonEncodeConst enc (val ::: ctp)
 where
   tupleList :: TypedValue (TypeRep f) -> [TypedValue (TypeRep f)]
   tupleList (x ::: Tp.Iso p t)   = tupleList (to p x ::: t)
   tupleList (p ::: Tp.Pair t1 t2) =
      tupleList (fst p ::: t1) ++ tupleList (snd p ::: t2)
   tupleList (x ::: Tag s t)
      | s `elem` ["ruletext", "message", "accept"] = tupleList (x ::: t)
   tupleList tv = [tv]

{-
jsonEncoderMap :: (a -> JSON) -> EncoderMap (Const a) EvalJSON JSON
jsonEncoderMap enc = M.fromList
   [ ("state", \(val ::: tp) -> do
        conv <- equalM (Tag "state" tp) stateType
        return (encodeState enc (conv val)))
   , ("Result", \(val ::: tp) -> do
        conv <- equalM (Tag "Result" tp) submitType
        encodeResult enc (conv val))
   , ("Exception", \(val ::: tp) -> do
        f <- equalM tp stringType
        fail (f val))
   ] -}

jsonEncodeConst :: (a -> JSON) -> Encoder (Const a) EvalJSON JSON
jsonEncodeConst enc (val ::: tp) =
   case tp of
      Rule      -> return (toJSON (showId val))
      Context   -> liftM enc (fromContext val)
      Term      -> return (enc val)
      BindingTp -> return $
                    Object [(showId val, String (showValue val))]
      Text      -> return (toJSON (show val))
      Int       -> return (toJSON val)
      Bool      -> return (toJSON val)
      Tp.String -> return (toJSON val)
      _         -> fail $ "Type " ++ show tp ++ " not supported in JSON"

instance Monoid JSON where
   mempty = Null
   mappend Null a = a
   mappend a Null = a
   mappend (Array xs) (Array ys) = Array (xs ++ ys)
   mappend (Array xs) b = Array (xs ++ [b])
   mappend a (Array xs) = Array (a:xs)
   mappend (Object xs) (Object ys) | distinct (map fst (xs ++ ys))
      = Object (xs ++ ys)
   mappend x y = Array [x, y]

jsonDecoder :: Exercise a -> Decoder (Type a) EvalJSON
jsonDecoder ex = Decoder (decode ex)
 where
   reader :: Exercise a -> EvalJSON a
   reader ex = do
      json <- get
      case json of
         String s -> either (fail . show) return (parser ex s)
         _        -> fail "Expecting a string when reading a term"

   decode :: Exercise a -> Type a t -> EvalJSON t
   decode ex serviceType =
      case serviceType of
         Tp.Tag s t
            | s == "state" -> do
                 f <- equalM stateType serviceType
                 useFirst $ \json -> do
                    old <- get
                    put json
                    a <- decodeState ex (reader ex)
                    put old
                    return (f a)
            | s == "args" -> do
                 f <- equalM envType t
                 useFirst $ liftM f . decodeContext
            | s == "Location" -> do
                 f <- equalM (List intType) t 
                 liftM f (useFirst decodeLocation)
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
               Term     -> useFirst $ \json -> do
                           old <- get
                           put json
                           a <- reader ex
                           put old
                           return a
               Exercise -> 
                        do json <- get
                           case json of
                              Array (String _:rest) -> put (Array rest) >> return ex
                              _ -> return ex
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
               Exercise -> return ex
               Script   -> lift (defaultScript (getId ex))
               _        -> fail $ "No support for argument type: " ++ show serviceType

   useFirst :: (JSON -> EvalJSON a) -> EvalJSON a
   useFirst f = do
      json <- get
      case json of
         Array (x:xs) -> do
            a <- f x
            put (Array xs)
            return a
         _ -> fail "expecting an argument"

jsonToId :: Monad m => JSON -> m Id
jsonToId = liftM (newId :: String -> Id) . fromJSON

decodeLocation :: Monad m => JSON -> m [Int]
decodeLocation (String s) = readM s
decodeLocation _          = fail "expecting a string for a location"

--------------------------

encodeState :: (a -> JSON) -> State a -> JSON
encodeState f st = Array
   [ String (showId (exercise st))
   , String $ case statePrefixes st of
                 []  -> "NoPrefix"
                 [p] -> show p
                 _   -> "MultiPrefix"
   , f (stateTerm st)
   , encodeContext (stateContext st)
   ]

encodeContext :: Context a -> JSON
encodeContext = Object . map f . bindings
 where
   f a = (showId a, String $ showValue a)

decodeState :: Exercise a -> EvalJSON a -> EvalJSON (State a)
decodeState ex f = do
   json <- get
   rec json
 where
   rec (Array [a]) = rec a
   rec (Array [String _code, String p, ce, jsonContext]) = do
      let mpr = readM p >>= (`makePrefix` strategy ex)
      a    <- do old <- get 
                 put ce 
                 a <- f
                 put old
                 return a
      env  <- decodeContext jsonContext
      return $ makeState ex mpr (makeContext ex env a)
   rec s = fail $ "invalid state" ++ show s

decodeContext :: Monad m => JSON -> m Environment
decodeContext (String "") = decodeContext (Object []) -- Being backwards compatible (for now)
decodeContext (Object xs) = foldM (flip add) mempty xs
 where
   add :: Monad m => (String, JSON) -> Environment -> m Environment
   add (k, String s) = return . insertRef (makeRef k) s
   add _             = fail "invalid item in context"
decodeContext json = fail $ "invalid context: " ++ show json

encodeResult :: (a -> JSON) -> Result a -> EvalJSON JSON
encodeResult enc result =
   case result of
      -- SyntaxError _ -> [("result", String "SyntaxError")]
      Buggy rs      -> return $ Object [("result", String "Buggy"), ("rules", Array $ map (String . showId) rs)]
      NotEquivalent -> return $ Object [("result", String "NotEquivalent")]
      Ok rs st      -> do
         json <- jsonEncode enc (st ::: stateType)
         return $ Object [("result", String "Ok"), ("rules", Array $ map (String . showId) rs), ("state", json)]
      Detour rs st  -> do
         json <- jsonEncode enc (st ::: stateType)
         return $ Object [("result", String "Detour"), ("rules", Array $ map (String . showId) rs), ("state", json)]
      Unknown st    -> do
         json <- jsonEncode enc (st ::: stateType)
         return $ Object [("result", String "Unknown"), ("state", json)]

jsonTuple :: [JSON] -> JSON
jsonTuple xs =
   case mapM f xs of
      Just ys | distinct (map fst ys) -> Object ys
      _ -> Array xs
 where
   f (Object [p]) = Just p
   f _ = Nothing