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
import Common.Utils (Some(..), safeHead)
import Common.Exercise
import Common.Strategy (makePrefix)
import Common.Transformation hiding (ruleList)
import Service.JSON
-- import Service.AbstractService
import qualified Service.TypedAbstractService as TAS
import Service.ServiceList hiding (Service)
import qualified Service.ExerciseList as List
import Control.Monad
import Data.List (sortBy)
import Data.Maybe
import Data.Char
import System.IO.Unsafe

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
   
myHandler :: JSON_RPC_Handler
myHandler fun arg
   | fun == "exerciselist" = exerciseList arg
   | fun == "rulelist"     = ruleList arg
   | otherwise = 
        case jsonConverter (extractCode arg) of
           Some conv -> do
              service <- getService fun
              return (execute service conv arg)

jsonConverter :: ExerciseCode -> Some (Converter JSON)
jsonConverter code = 
   case List.getExercise code of
      Just (Some ex) -> Some $ 
         let builder = String . prettyPrinter ex
             reader (String s) = either (const Nothing) Just (parser ex s)
             reader _ = fail "reading term" 
         in Converter 
                  { exercise = ex
                  , toTerm   = reader
                  , fromTerm = builder
                  , toType   = toArgument ex reader
                  , fromType = fromResult builder
                  }
      _ -> error "exercise code not found"



{-
instance InJSON a => InJSON (Context a) where
   toJSON = toJSON . fromContext
   fromJSON a = fromJSON a >>= (return . inContext)

instance InJSON ExerciseCode where 
   toJSON = toJSON . show
   fromJSON (String s) = List.resolveExerciseCode s
   fromJSON _          = fail "expecting a string"  -}
   
instance InJSON Location where
   toJSON              = toJSON . show
   fromJSON (String s) = case reads s of
                            [(loc, rest)] | all isSpace rest -> return loc
                            _ -> fail "invalid string"
   fromJSON _          = fail "expecting a string"

instance InJSON (Rule a) where
   toJSON = toJSON . name

--------------------------

toArgument :: Exercise a -> (JSON -> Maybe a) -> ServiceType a t -> JSON -> t
toArgument ex f (PairType t1 t2) (Array [a, b]) =
   (toArgument ex f t1 a, toArgument ex f t2 b)
toArgument ex f (TripleType t1 t2 t3) (Array [a, b, c]) =
   (toArgument ex f t1 a, toArgument ex f t2 b, toArgument ex f t3 c)
toArgument ex f serviceType json =
   case serviceType of 
      StateType    -> jsonToState ex f json
      LocationType -> fromJust $ fromJSON json
      TermType     -> inContext (fromJust (f json))
      RuleType     -> fromJust $ getRule ex $ fromJust $ fromJSON json
      ExerciseType -> ex
      _            -> error "toArgument"
       
fromResult :: (a -> JSON) -> ServiceType a t -> t -> JSON
fromResult f serviceType tv =
   case serviceType of
      ListType t -> Array $ map (fromResult f t) tv
      PairType t1 t2 -> 
         let (a, b) = tv
         in Array [fromResult f t1 a, fromResult f t2 b]
      TripleType t1 t2 t3 -> 
         let (a, b,c ) = tv
         in Array [fromResult f t1 a, fromResult f t2 b, fromResult f t3 c]
      ElemType t -> 
         fromResult f t tv
      IOType t -> 
         fromResult f t (unsafePerformIO tv) -- quick fix
      IntType -> toJSON tv
      BoolType -> toJSON tv
      StringType -> toJSON tv
      LocationType -> toJSON tv
      RuleType -> toJSON (name tv)
      StateType -> stateToJSON f tv
      ResultType -> resultToJSON f tv
      TermType -> f (fromContext tv)

jsonToState :: Exercise a -> (JSON -> Maybe a) -> JSON -> TAS.State a
jsonToState ex f (Array [a]) = jsonToState ex f a
jsonToState ex f (Array [String code, String p, ce, String ctx]) = 
   case (f ce, parseContext ctx) of 
      (Just a, Just unit) -> TAS.State 
         { TAS.exercise = ex
         , TAS.prefix   = fmap (`makePrefix` strategy ex) (readPrefix p) 
         , TAS.context  = fmap (\_ -> a) unit
         }
      _ -> error "jsonToState"
jsonToState _ _ a = error $ "jsonToState: " ++ show a

readPrefix :: String -> Maybe [Int]
readPrefix input =
   case reads input of
      [(is, rest)] | all isSpace rest -> return is
      _ -> Nothing

stateToJSON :: (a -> JSON) -> TAS.State a -> JSON
stateToJSON f state = Array
   [ String $ show $ exerciseCode (TAS.exercise state)
   , String $ maybe "NoPrefix" show (TAS.prefix state)
   , f (TAS.term state)
   , String $ showContext (TAS.context state)
   ] 
   
resultToJSON :: (a -> JSON) -> TAS.Result a -> JSON
resultToJSON f result = Object $
   case result of
      -- TAS.SyntaxError _ -> [("result", String "SyntaxError")]
      TAS.Buggy rs      -> [("result", String "Buggy"), ("rules", Array $ map toJSON rs)]
      TAS.NotEquivalent -> [("result", String "NotEquivalent")]   
      TAS.Ok rs st      -> [("result", String "Ok"), ("rules", Array $ map toJSON rs), ("state", stateToJSON f  st)]
      TAS.Detour rs st  -> [("result", String "Detour"), ("rules", Array $ map toJSON rs), ("state", stateToJSON f  st)]
      TAS.Unknown st    -> [("result", String "Unknown"), ("state", stateToJSON f st)]
      
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