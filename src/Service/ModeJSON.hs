{-# OPTIONS -XRankNTypes #-}
-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
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
module Service.ModeJSON (processJSON) where

import Common.Context
import Common.Utils (Some(..))
import Common.Exercise
import Common.Strategy (makePrefix)
import Common.Transformation hiding (ruleList, defaultArgument)
import Service.JSON
import Service.Types (Evaluator(..), Type, encodeDefault, decodeDefault, Encoder(..), Decoder(..))
import qualified Service.Types as Tp
import qualified Service.TypedAbstractService as TAS
import Service.ServiceList hiding (Service)
import qualified Service.ExerciseList as List
import Control.Monad
import Data.List (sortBy)
import Data.Maybe
import Data.Char

extractCode :: JSON -> ExerciseCode
extractCode = fromMaybe (makeCode "" "") . List.resolveExerciseCode . f
 where 
   f (String s) = s
   f (Array [String _, String _, a@(Array _)]) = f a
   f (Array (String s:tl)) | any (\c -> not (isAlphaNum c || isSpace c)) s = f (Array tl)
   f (Array (hd:_)) = f hd
   f _ = ""
      
processJSON :: String -> IO (String, String)
processJSON input = do
   txt <- jsonRPC input myHandler
   return (txt, "application/json")

{- jsonRequest :: JSON -> Either String Request
jsonRequest json = do
   srv  <- case lookupM "method" json of
              Just (String s) -> return s
              _               -> fail "Invalid method"
   code <- lookupM "params" json >>= (return . extractCode)
   enc  <- case lookupM "encoding" json of
              Nothing         -> return Nothing
              Just (String s) -> liftM Just (readEncoding s)
              _               -> fail "Invalid encoding"
   src  <- case lookupM "source" json of
              Nothing         -> return Nothing
              Just (String s) -> return (Just s)
              _               -> fail "Invalid source"
   return $ Request 
      { service    = srv
      , exerciseID = code
      , source     = src
      , dataformat = JSON
      , encoding   = enc
      }

jsonReply :: Request -> JSON -> Either String JSON
jsonReply request json
   | otherwise =  -}

myHandler :: JSON_RPC_Handler
myHandler fun arg
   | fun == "exerciselist" = exerciseList arg
   | fun == "rulelist"     = ruleList arg
   | otherwise = 
        case jsonConverter (extractCode arg) of
           Some conv -> do
              service <- getService fun
              case evalService conv service arg of
                 Left err  -> fail err
                 Right txt -> return txt

jsonConverter :: ExerciseCode -> Some (Evaluator (Either String) JSON JSON)
jsonConverter code =
   case List.getExercise code of
      Just (Some ex) -> 
         Some (Evaluator (jsonEncoder ex) (jsonDecoder ex))
      _ -> error "exercise code not found"

jsonEncoder :: Exercise a -> Encoder JSON a
jsonEncoder ex = Encoder
   { encodeType  = encode (jsonEncoder ex)
   , encodeTerm  = String . prettyPrinter ex
   , encodeTuple = Array
   }
 where
   encode :: Encoder JSON a -> Type a t -> t -> JSON
   encode enc serviceType =
      case serviceType of
         Tp.List t   -> Array . map (encode enc t)
         Tp.Elem t   -> encode enc t
         Tp.Int      -> toJSON
         Tp.Bool     -> toJSON
         Tp.String   -> toJSON
         Tp.Location -> toJSON
         Tp.Rule     -> toJSON . name
         Tp.State    -> encodeState (encodeTerm enc)
         Tp.Result   -> encodeResult (encodeTerm enc)
         Tp.Term     -> encodeTerm enc . fromContext
         _           -> encodeDefault enc serviceType

jsonDecoder :: Monad m => Exercise a -> Decoder m JSON a
jsonDecoder ex = Decoder
   { decodeType      = decode (jsonDecoder ex)
   , decodeTerm      = reader ex
   , decoderExercise = ex
   }
 where
   reader :: Monad m => Exercise a -> JSON -> m a
   reader ex (String s) = either (fail . show) return (parser ex s)
   reader _  _          = fail "Expecting a string when reading a term"
 
   decode :: Monad m => Decoder m JSON a -> Type a t -> JSON -> m (t, JSON) 
   decode dec serviceType =
      case serviceType of
         Tp.State    -> useFirst $ decodeState (decoderExercise dec) (decodeTerm dec)
         Tp.Location -> useFirst fromJSON
         Tp.Term     -> useFirst $ liftM inContext . decodeTerm dec
         Tp.Rule     -> useFirst $ \x -> fromJSON x >>= getRule (decoderExercise dec)
         Tp.Exercise -> \json -> return (decoderExercise dec, json)
         _           -> decodeDefault dec serviceType
   
   useFirst :: Monad m => (JSON -> m a) -> JSON -> m (a, JSON)
   useFirst f (Array (x:xs)) = do
      a <- f x
      return (a, Array xs)
   useFirst _ _ = fail "expecting an arugment"
         
instance InJSON Location where
   toJSON              = toJSON . show
   fromJSON (String s) = case reads s of
                            [(loc, rest)] | all isSpace rest -> return loc
                            _ -> fail "invalid string"
   fromJSON _          = fail "expecting a string"

instance InJSON (Rule a) where
   toJSON = toJSON . name

--------------------------

decodeState :: Monad m => Exercise a -> (JSON -> m a) -> JSON -> m (TAS.State a)
decodeState ex f (Array [a]) = decodeState ex f a
decodeState ex f (Array [String code, String p, ce, String ctx]) = do
   a    <- f ce 
   unit <- maybe (fail "invalid context") return (parseContext ctx) 
   return $ TAS.State 
      { TAS.exercise = ex
      , TAS.prefix   = fmap (`makePrefix` strategy ex) (readPrefix p) 
      , TAS.context  = fmap (\_ -> a) unit
      }
decodeState _ _ s = fail $ "invalid state" ++ show s

readPrefix :: String -> Maybe [Int]
readPrefix input =
   case reads input of
      [(is, rest)] | all isSpace rest -> return is
      _ -> Nothing

encodeState :: (a -> JSON) -> TAS.State a -> JSON
encodeState f state = Array
   [ String $ show $ exerciseCode (TAS.exercise state)
   , String $ maybe "NoPrefix" show (TAS.prefix state)
   , f (TAS.term state)
   , String $ showContext (TAS.context state)
   ] 
   
encodeResult :: (a -> JSON) -> TAS.Result a -> JSON
encodeResult f result = Object $
   case result of
      -- TAS.SyntaxError _ -> [("result", String "SyntaxError")]
      TAS.Buggy rs      -> [("result", String "Buggy"), ("rules", Array $ map toJSON rs)]
      TAS.NotEquivalent -> [("result", String "NotEquivalent")]   
      TAS.Ok rs st      -> [("result", String "Ok"), ("rules", Array $ map toJSON rs), ("state", encodeState f  st)]
      TAS.Detour rs st  -> [("result", String "Detour"), ("rules", Array $ map toJSON rs), ("state", encodeState f  st)]
      TAS.Unknown st    -> [("result", String "Unknown"), ("state", encodeState f st)]

------------------------------------------------------------
-- to be removed

exerciseList :: JSON -> IO JSON
exerciseList _ = return $ Array $ map make $ sortBy cmp List.exerciseList
 where
   cmp e1 e2  = f e1 `compare` f e2
   f (Some e) = (domain e, identifier e)
   make (Some ex) = Object
      [ ("domain",      String $ domain ex)
      , ("identifier",  String $ identifier ex)
      , ("description", String $ description ex)
      , ("status",      String $ show $ status ex)
      ]

ruleList :: JSON -> IO JSON
ruleList (Array [String code]) =
   case filter p List.exerciseList of
      [Some ex] -> return $ Array $ map f (ruleset ex)
      _ -> fail "unknown exercise code"
 where
   p (Some ex) = show (exerciseCode ex) == code
   f r = Object 
      [ ("name",        String  $ name r)
      , ("buggy",       Boolean $ isBuggyRule r) 
      , ("rewriterule", Boolean $ isRewriteRule r)
      ]
ruleList xs = fail $ "ruleList service requires exactly one argument (with a string)" ++ show xs