{-# LANGUAGE GADTs, FlexibleInstances #-}
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

module Ideas.Encoding.DecoderJSON
   ( JSONDecoder, jsonTypeDecoder
   ) where

import Control.Monad.State (get, gets)
import Data.Char
import Data.Maybe
import Ideas.Common.Library hiding (exerciseId, symbol)
import Ideas.Common.Traversal.Navigator
import Ideas.Encoding.Encoder
import Ideas.Encoding.Options
import Ideas.Service.State
import Ideas.Service.Types hiding (String)
import Ideas.Text.JSON
import qualified Ideas.Service.Types as Tp

type JSONDecoder a = GDecoderJSON (Exercise a, Options)

jsonTypeDecoder :: TypedDecoder a JSON
jsonTypeDecoder tp = do
   env  <- reader id
   json <- get
   case evalGDecoderJSON (jArray (decodeType tp)) env json of 
      Left err -> throwError (show err)
      Right a  -> return a

decodeType :: Type a t -> JSONDecoder a t
decodeType tp =
   case tp of
      Tag _ t -> decodeType t
      Iso p t -> from p <$> decodeType t
      Pair t1 t2 -> do
         a <- decodeType t1
         b <- decodeType t2
         return (a, b)
      t1 :|: t2 ->
         Left  <$> decodeType t1 <|>
         Right <$> decodeType t2
      Unit         -> return ()
      Const QCGen  -> getQCGen
      Const Script -> getScript
      Const t      -> decodeConst t
      _ -> errorStr $ "No support for argument type: " ++ show tp

decodeConst :: Const a t -> JSONDecoder a t
decodeConst tp =
   case tp of
      State       -> decodeState
      Context     -> decodeContext
      Exercise    -> getExercise <* jSkip
      Environment -> decodeEnvironment
      Location    -> decodeLocation
      Term        -> gets (jsonToTerm . toJSON)
      Int         -> jInt
      Tp.String   -> jString
      Id          -> decodeId
      Rule        -> decodeRule
      _           -> errorStr $ "No support for argument type: " ++ show tp

decodeRule :: JSONDecoder a (Rule (Context a))
decodeRule = do
   ex  <- getExercise
   rid <- newId <$> jString
   case getRule ex rid of
      Just r  -> return r
      Nothing -> errorStr ("unknown rule " ++ show rid) 

decodeId :: JSONDecoder a Id
decodeId = newId <$> jString

decodeLocation :: JSONDecoder a Location
decodeLocation = jString >>= \s -> 
   case readM s of
      Just is -> return (toLocation is)
      Nothing -> errorStr "invalid location"

decodeState :: JSONDecoder a (State a)
decodeState = jArray1 decodeState <|> jArray content
 where
   content = do
      ex  <- getExercise
      _   <- jString -- exercise id
      pts <- decodePaths
      a   <- decodeExpression
      env <- decodeEnvironment
      let loc = envToLoc env
          ctx = navigateTowards loc $ deleteRef locRef $
                   setEnvironment env $ inContext ex a
          prfx = pts (strategy ex) ctx
          defState = makeState ex prfx ctx
      
      (extra <|> id <$ jEmpty) <*> pure defState

   extra = jArray3 f jString jString jString
    where
      f user session startterm st = st
         { stateUser      = Just user
         , stateSession   = Just session
         , stateStartTerm = Just startterm
         }

envToLoc :: Environment -> Location
envToLoc env = toLocation $ fromMaybe [] $ locRef ? env >>= readM

locRef :: Ref String
locRef = makeRef "location"

decodePaths :: JSONDecoder a (LabeledStrategy (Context a) -> Context a -> Prefix (Context a))
decodePaths = jString >>= \s -> 
   case readPaths s of
      _ | s ~= "noprefix" 
              -> return (\_ _ -> noPrefix)
      Just ps -> return (replayPaths ps)
      Nothing -> errorStr "invalid path"
 where
   x ~= y = filter isAlphaNum (map toLower x) == y

decodeEnvironment :: JSONDecoder a Environment
decodeEnvironment = foldr ($) mempty <$> jObjectWithKeys f <|> mempty <$ jString {- only accept "" -} 
 where
   f k = g <$> jString <|> g . show <$> jInt
    where
      g = insertRef (makeRef k)

decodeContext :: JSONDecoder a (Context a)
decodeContext = inContext <$> getExercise <*> decodeExpression

decodeExpression :: JSONDecoder a a
decodeExpression = withJSONTerm $ \b -> 
   if b
   then do
      mv <- hasJSONView <$> getExercise
      case mv of 
         Just v  -> jNext (maybe (Left "cannot decode expression from JSON") Right . match v)
         Nothing -> errorStr "JSON encoding not supported by exercise"
   else do
      ex <- getExercise
      jString >>= either errorStr return . parser ex