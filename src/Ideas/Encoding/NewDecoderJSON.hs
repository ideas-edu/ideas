{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
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

module Ideas.Encoding.NewDecoderJSON
   ( JSONDecoder, jsonTypeDecoder
   ) where

import Control.Monad.State (get)
import Data.Char
import Ideas.Common.Library hiding (exerciseId, symbol)
import Ideas.Common.Traversal.Navigator
import Ideas.Encoding.Encoder
import Ideas.Encoding.Options
import Ideas.Service.State
import Ideas.Service.Types hiding (String)
import Ideas.Text.JSON
import qualified Ideas.Service.Types as Tp

type JSONDecoder a = GDecoderJSON (Exercise a, Options)

jsonTypeDecoder :: Type a t -> DecoderX a String JSON t
jsonTypeDecoder tp = do
    env  <- reader id
    json <- get
    case evalGDecoderJSON (jObject $ decodeType tp) env json of
       Left err -> throwError (show err)
       Right a  -> return a

decodeType :: Type a t -> JSONDecoder a t
decodeType tp =
   case tp of
      Tag s t ->
         jKey (map toLower s) $ decodeType t
      Iso p t -> from p <$> decodeType t
      Pair t1 t2 -> do
         a <- decodeType t1
         b <- decodeType t2
         return (a, b)
      t1 :|: t2 ->
         Left  <$> decodeType t1 <|>
         Right <$> decodeType t2
      Unit         -> return ()
      Const t -> decodeConst t
      _ -> errorStr $ "No support for argument type: " ++ show tp

decodeConst :: Const a t -> JSONDecoder a t
decodeConst tp =
   case tp of
      State       -> decodeState
      Context     -> decodeContext
      Exercise    -> getExercise
      Environment -> decodeEnvironment
      Location    -> decodeLocation
      Script      -> getScript
      Int         -> jInt
      Tp.String   -> jString
      Id          -> decodeId
      Rule        -> decodeRule
      QCGen       -> getQCGen
      _           -> errorStr $ "No support for argument type: " ++ show tp

decodeState :: JSONDecoder a (State a)
decodeState = jKey "state" $ jObject $ do
   ex  <- getExercise
   ps  <- decodePaths
   ctx <- decodeContext
   let prf = maybe noPrefix (\x -> replayPaths x (strategy ex) ctx) ps
   uid <- searchKey "userid"
   sid <- searchKey "sessionid"
   tid <- searchKey "taskid"
   return $ (makeState ex prf ctx)
      { stateUser      = uid
      , stateSession   = sid
      , stateStartTerm = tid
      }

searchKey :: String -> JSONDecoder a (Maybe String)
searchKey k = Just <$> jKey k jString <|> return Nothing

decodePaths :: JSONDecoder a (Maybe [Path])
decodePaths = f <$> jKey "prefix" jString
 where
   f s | s ~= "noprefix" = Nothing
       | otherwise       = readPaths s
   x ~= y = filter isAlphaNum (map toLower x) == y

decodeContext :: JSONDecoder a (Context a)
decodeContext = jKey "context" $ jObject $ do
   ex  <- getExercise
   val <- decodeValue
   env <- decodeEnvironment
   loc <- decodeLocation
   return $ navigateTowards loc $ deleteRef locRef $
      setEnvironment env $ inContext ex val

decodeValue :: JSONDecoder a a
decodeValue = jKey "term" $ decodeParser <|> decodeJSON
 where
   decodeParser = do
      ex <- getExercise
      s <- jString
      either errorStr return (parser ex s)

   decodeJSON = do
      ex <- getExercise
      case hasJSONView ex of 
         Just jv -> jNext $ maybe (Left "Could not decode term") Right . match jv
         Nothing -> errorStr "No JSON decoder for term"

decodeEnvironment :: JSONDecoder a Environment
decodeEnvironment = jKey "environment" $ foldr ($) mempty <$> jObjectWithKeys f
 where
   f k = g <$> jString <|> g . show <$> jInt <|> g . show <$> jDouble
    where
      g = insertRef (makeRef k)

decodeLocation :: JSONDecoder a Location
decodeLocation = jKey "location" $ 
   toLocation <$> jArrayOf jInt

decodeRule :: JSONDecoder a (Rule (Context a))
decodeRule = do
   ex <- getExercise
   jKey "rule" $ do 
      rid <- newId <$> jString
      case getRule ex rid of
         Just a  -> return a
         Nothing -> errorStr ("unknown rule " ++ show rid) 

decodeId :: JSONDecoder a Id -- fix me
decodeId = jKey "rule" $ newId <$> jString

locRef :: Ref String
locRef = makeRef "location"
