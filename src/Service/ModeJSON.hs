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
import Data.Char
import Service.DomainReasoner
import Service.Evaluator
import Service.Request
import Service.State
import Service.Submit
import Service.Types hiding (String)
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
   ex   <- if fun == "exerciselist"
           then return (Some emptyExercise)
           else extractExerciseId arg >>= findExercise
   srv  <- findService fun
   case jsonConverter ex of
      Some conv ->
         evalService conv srv arg

jsonConverter :: Some Exercise -> Some (Evaluator JSON JSON)
jsonConverter (Some ex) =
   Some (Evaluator (jsonEncoder ex) (jsonDecoder ex))

jsonEncoder :: Exercise a -> Encoder JSON a
jsonEncoder ex = jsonEncode (String . prettyPrinter ex)

jsonEncode :: (a -> JSON) -> Encoder JSON a
jsonEncode enc tp a
   | length xs > 1 =
        liftM jsonTuple (mapM (\(b ::: t) -> jsonEncode enc t b) xs)
   | otherwise =
        case tp of
           Iso p t   -> jsonEncode enc t (to p a)
           t1 :|: t2 -> case a of
              Left  x -> jsonEncode enc t1 x
              Right y -> jsonEncode enc t2 y
           Pair t1 t2 -> do
              x <- jsonEncode enc t1 (fst a)
              y <- jsonEncode enc t2 (snd a)
              return (jsonTuple [x, y])
           Tp.Tag s t
              | s `elem` ["elem", "list"] ->
                   jsonEncode enc t a
              | s == "Result" -> do
                   conv <- equalM tp submitType
                   encodeResult enc (conv a)
              | s == "state" -> do
                   conv <- equalM tp stateType
                   return (encodeState enc (conv a))
              | otherwise -> liftM (\b -> Object [(s, b)]) (jsonEncode enc t a)
           Tp.Rule       -> return (toJSON (showId a))
           Tp.Context    -> liftM enc (fromContext a)
           Tp.Term       -> return (enc a)
           Tp.Unit       -> return Null
           Tp.IO t       -> do x <- liftIO (runIO a)
                               jsonEncode enc (Exception :|: t) x
           Tp.Exception  -> fail a
           Tp.Location   -> return (toJSON (show a))
           Tp.List t     -> liftM Array (mapM (jsonEncode enc t) a)
           Tp.BindingTp  -> return $
                               Object [(showId a, String (showValue a))]
           Tp.Text       -> return (toJSON (show a))
           Tp.Int        -> return (toJSON a)
           Tp.Bool       -> return (toJSON a)
           Tp.String     -> return (toJSON a)
           _             -> fail $ "Type " ++ show tp ++ " not supported in JSON"
 where
   xs = tupleList (a ::: tp)

   tupleList :: TypedValue a -> [TypedValue a]
   tupleList (x ::: Tp.Iso p t)   = tupleList (to p x ::: t)
   tupleList (p ::: Tp.Pair t1 t2) =
      tupleList (fst p ::: t1) ++ tupleList (snd p ::: t2)
   tupleList (x ::: Tag s t)
      | s `elem` ["ruletext", "message", "accept"] = tupleList (x ::: t)
   tupleList tv = [tv]

jsonDecoder :: Exercise a -> Decoder JSON a
jsonDecoder ex = Decoder
   { decodeType      = decode (jsonDecoder ex)
   , decodeTerm      = reader (parser ex)
   , decoderExercise = ex
   }
 where
   reader :: Monad m => (String -> Either String a) -> JSON -> m a
   reader f (String s) = either (fail . show) return (f s)
   reader _  _         = fail "Expecting a string when reading a term"

   decode :: Decoder JSON a -> Type a t -> JSON -> DomainReasoner (t, JSON)
   decode dec serviceType =
      case serviceType of
         Tp.Location -> useFirst decodeLocation
         Tp.Term     -> useFirst $ decodeTerm dec
         Tp.Rule     -> useFirst $ \x -> jsonToId x >>= getRule (decoderExercise dec)
         Tp.Exercise -> \json -> case json of
                                       Array (String _:rest) -> return (decoderExercise dec, Array rest)
                                       _ -> return (decoderExercise dec, json)
         Tp.Int      -> useFirst $ \json -> case json of
                                               Number (I n) -> return (fromIntegral n)
                                               _        -> fail "not an integer"
         Tp.String   -> useFirst $ \json -> case json of
                                               String s -> return s
                                               _        -> fail "not a string"
         Tp.Tag s t
            | s == "state" -> do
                 f <- equalM stateType serviceType
                 useFirst (liftM f . decodeState (decoderExercise dec) (decodeTerm dec))
            | s == "args" -> do
                 f <- equalM envType t
                 useFirst $ liftM f . decodeContext
         _ -> decodeDefault dec serviceType

   useFirst :: Monad m => (JSON -> m a) -> JSON -> m (a, JSON)
   useFirst f (Array (x:xs)) = do
      a <- f x
      return (a, Array xs)
   useFirst _ _ = fail "expecting an argument"

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

decodeState :: Monad m => Exercise a -> (JSON -> m a) -> JSON -> m (State a)
decodeState ex f (Array [a]) = decodeState ex f a
decodeState ex f (Array [String _code, String p, ce, jsonContext]) = do
   let mpr = readM p >>= (`makePrefix` strategy ex)
   a    <- f ce
   env  <- decodeContext jsonContext
   return $ makeState ex mpr (makeContext ex env a)
decodeState _ _ s = fail $ "invalid state" ++ show s

decodeContext :: Monad m => JSON -> m Environment
decodeContext (String "") = decodeContext (Object []) -- Being backwards compatible (for now)
decodeContext (Object xs) = foldM (flip add) mempty xs
 where
   add :: Monad m => (String, JSON) -> Environment -> m Environment
   add (k, String s) = return . insertRef (makeRef k) s
   add _             = fail "invalid item in context"
decodeContext json = fail $ "invalid context: " ++ show json

encodeResult :: (a -> JSON) -> Result a -> DomainReasoner JSON
encodeResult enc result =
   case result of
      -- SyntaxError _ -> [("result", String "SyntaxError")]
      Buggy rs      -> return $ Object [("result", String "Buggy"), ("rules", Array $ map (String . showId) rs)]
      NotEquivalent -> return $ Object [("result", String "NotEquivalent")]
      Ok rs st      -> do
         json <- jsonEncode enc stateType st
         return $ Object [("result", String "Ok"), ("rules", Array $ map (String . showId) rs), ("state", json)]
      Detour rs st  -> do
         json <- jsonEncode enc stateType st
         return $ Object [("result", String "Detour"), ("rules", Array $ map (String . showId) rs), ("state", json)]
      Unknown st    -> do
         json <- jsonEncode enc stateType st
         return $ Object [("result", String "Unknown"), ("state", json)]

jsonTuple :: [JSON] -> JSON
jsonTuple xs =
   case mapM f xs of
      Just ys | distinct (map fst ys) -> Object ys
      _ -> Array xs
 where
   f (Object [p]) = Just p
   f _ = Nothing