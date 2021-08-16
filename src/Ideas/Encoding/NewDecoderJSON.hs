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
   ( JSONDecoder, jsonDecoder
   ) where

import Control.Monad.State (mplus, foldM, get, put)
import Data.Char
import Ideas.Common.Library hiding (exerciseId, symbol)
import Ideas.Common.Traversal.Navigator
import Ideas.Encoding.Encoder
import Ideas.Service.State
import Ideas.Service.Types hiding (String)
import Ideas.Text.JSON
import qualified Ideas.Service.Types as Tp

type JSONDecoder a t = DecoderX a JSON t

jsonDecoder :: TypedDecoder a JSON
jsonDecoder tp = 
   case tp of
      Tag s t -> do
         this <- get
         case lookupM (map toLower s) this of
            Just json -> do
               put json
               a <- jsonDecoder t
               put this
               return a
            Nothing -> fail $ "Could not find " ++ s
      Iso p t -> from p <$> jsonDecoder t
      Pair t1 t2 -> do
         a <- jsonDecoder t1
         b <- jsonDecoder t2
         return (a, b)
      t1 :|: t2 ->
         (Left  <$> jsonDecoder t1) `mplus`
         (Right <$> jsonDecoder t2)
      Unit         -> return ()
      Const t -> decodeConst t
      _ -> fail $ "No support for argument type: " ++ show tp

decodeConst :: Const a t -> JSONDecoder a t
decodeConst tp =
   case tp of
      State       -> decodeState
      Context     -> decodeContext
      Exercise    -> getExercise
      Environment -> decodeEnvironment
      Location    -> decodeLocation
      --Term        -> gets jsonToTerm
      Script      -> getScript
      Int         -> get >>= fromJSON
      Tp.String   -> get >>= fromJSON
      Id          -> decodeId
      Rule        -> decodeRule
      QCGen       -> getQCGen
      _           -> fail $ "No support for argument type: " ++ show tp


{-
jsonDecoder tp = get >>= \json ->
   case json of
      Array xs -> decodeType tp // xs
      _ -> fail "expecting an array"

decodeType :: Type a t -> DecoderX a [JSON] t
decodeType tp =
   case tp of
      Tag _ t -> decodeType t
      Iso p t -> from p <$> decodeType t
      Pair t1 t2 -> do
         a <- decodeType t1
         b <- decodeType t2
         return (a, b)
      t1 :|: t2 ->
         (Left  <$> decodeType t1) `mplus`
         (Right <$> decodeType t2)
      Unit         -> return ()
      Const QCGen  -> getQCGen
      Const Script -> getScript
      Const t      -> symbol >>= \a -> decodeConst t // a
      _ -> fail $ "No support for argument type: " ++ show tp

decodeConst :: Const a t -> JSONDecoder a t
decodeConst tp =
   case tp of
      State       -> decodeState
      Context     -> decodeContext
      Exercise    -> getExercise
      Environment -> decodeEnvironment
      Location    -> decodeLocation
      Term        -> gets jsonToTerm
      Int         -> get >>= fromJSON
      Tp.String   -> get >>= fromJSON
      Id          -> decodeId
      Rule        -> decodeRule
      _           -> fail $ "No support for argument type: " ++ show tp

decodeRule :: JSONDecoder a (Rule (Context a))
decodeRule = do
   ex <- getExercise
   get >>= \json ->
      case json of
         String s -> getRule ex (newId s)
         _        -> fail "expecting a string for rule"

decodeId :: JSONDecoder a Id
decodeId = get >>= \json ->
   case json of
      String s -> return (newId s)
      _        -> fail "expecting a string for id"

decodeLocation :: JSONDecoder a Location
decodeLocation = get >>= \json ->
   case json of
      String s -> toLocation <$> readM s
      _        -> fail "expecting a string for a location"
-}
{-
{   "service" : "allfirsts"
,   "source"  : "test.logex"
,   "state" : {
          "exerciseid" : "logic.propositional.dnf"
        , "prefix"     : "[]"
        , "context"    : {
              "term"        : "~(~x || ~y)"
            , "environment" : {}
            , "location"    : []
        }
        , "userid"    : null
        , "session"   : null
        , "startterm" : null
    }
}
-}

decodeState :: JSONDecoder a (State a)
decodeState = do
   this <- get
   case lookupM "state" this of
      Nothing -> fail "expecting state"
      Just json -> do
         put json
         ex  <- getExercise
         ps  <- decodePaths
         ctx <- decodeContext
         let prf = maybe noPrefix (\x -> replayPaths x (strategy ex) ctx) ps
             uid = searchId "userid"    json
             sid = searchId "sessionid" json
             tid = searchId "taskid"    json
         put this
         return $ (makeState ex prf ctx)
            { stateUser      = uid
            , stateSession   = sid
            , stateStartTerm = tid
            }

searchId :: String -> JSON -> Maybe String
searchId str json =
   case lookupM str json of 
      Just (String s) -> Just s
      _ -> Nothing

decodePaths :: JSONDecoder a (Maybe [Path])
decodePaths = do
   this <- get
   case lookupM "prefix" this of
      Just (String s)
         | s ~= "noprefix" -> return Nothing
         | otherwise       -> Just <$> readPaths s
      _ -> fail "expecting prefix"
 where
   x ~= y = filter isAlphaNum (map toLower x) == y

decodeContext :: JSONDecoder a (Context a)
decodeContext = do
   this <- get
   case lookupM "context" this of
      Just json -> do
         put json
         ex  <- getExercise
         val <- decodeValue
         env <- decodeEnvironment
         loc <- decodeLocation
         put this
         return $ navigateTowards loc $ deleteRef locRef $
                         setEnvironment env $ inContext ex val
      _ -> fail $ "expecting context: " ++ show this 

decodeValue :: JSONDecoder a a
decodeValue = do
   this <- get
   ex   <- getExercise
   case (lookupM "term" this, hasJSONView ex) of 
      (Just json, Just jv) -> matchM jv json 
      (Just (String s), _) -> either fail return (parser ex s)
      _ -> fail "Expecting term"

decodeEnvironment :: JSONDecoder a Environment
decodeEnvironment = do
   this <- get
   case lookupM "environment" this of 
      Just (Object xs) -> foldM (flip add) mempty xs
      _ -> fail "Expecting environment"
 where
   add (k, String s) = return . insertRef (makeRef k) s
   add (k, Number n) = return . insertRef (makeRef k) (show n)
   add _             = fail "invalid item in context"

decodeLocation :: JSONDecoder a Location
decodeLocation = do
   this <- get
   case lookupM "location" this of 
      Just (Array xs) -> toLocation <$> mapM f xs
      _ -> fail "Expecting location"
 where
   f (Number (I n)) = return (fromInteger n)
   f _ = fail "invalid int in location"

decodeRule :: JSONDecoder a (Rule (Context a))
decodeRule = do
   ex <- getExercise
   get >>= \json ->
      case lookupM "rule" json of
         Just (String s) -> getRule ex (newId s)
         _        -> fail "expecting a string for rule"

decodeId :: JSONDecoder a Id -- fix me
decodeId = do
   get >>= \json ->
      case lookupM "rule" json of
         Just (String s) -> return (newId s)
         _        -> fail "expecting a string for rule"




      -- return (toLocation [])

 {-
   ex <- getExercise
   get >>= \json ->
      case json of
         Array [a] -> put a >> decodeState
         Array (String _code : pref : term : jsonContext : rest) -> do
            pts  <- decodePaths       // pref
            a    <- decodeExpression  // term
            env  <- decodeEnvironment // jsonContext
            let loc = envToLoc env
                ctx = navigateTowards loc $ deleteRef locRef $
                         setEnvironment env $ inContext ex a
                prfx = pts (strategy ex) ctx
            case rest of
               [] -> return $ makeState ex prfx ctx
               [Array [String user, String session, String startterm]] ->
                  return (makeState ex prfx ctx)
                     { stateUser      = Just user
                     , stateSession   = Just session
                     , stateStartTerm = Just startterm
                     }
               _  -> fail $ "invalid state" ++ show json
         _ -> fail $ "invalid state" ++ show json

envToLoc :: Environment -> Location
envToLoc env = toLocation $ fromMaybe [] $ locRef ? env >>= readM
-}
locRef :: Ref String
locRef = makeRef "location"
{-
decodePaths :: JSONDecoder a (LabeledStrategy (Context a) -> Context a -> Prefix (Context a))
decodePaths =
   get >>= \json ->
      case json of
         String p
            | p ~= "noprefix" -> return (\_ _ -> noPrefix)
            | otherwise       -> replayPaths <$> readPaths p
         _ -> fail "invalid prefixes"
 where
   x ~= y = filter isAlphaNum (map toLower x) == y

decodeEnvironment :: JSONDecoder a Environment
decodeEnvironment = get >>= \json ->
   case json of
      String "" -> return mempty
      Object xs -> foldM (flip add) mempty xs
      _         -> fail $ "invalid context: " ++ show json
 where
   add (k, String s) = return . insertRef (makeRef k) s
   add (k, Number n) = return . insertRef (makeRef k) (show n)
   add _             = fail "invalid item in context"

decodeContext :: JSONDecoder a (Context a)
decodeContext = do
   ex <- getExercise
   inContext ex <$> decodeExpression

decodeExpression :: JSONDecoder a a
decodeExpression = withJSONTerm $ \b -> getExercise >>= \ex -> get >>= f b ex
 where
   f True ex json =
      case hasJSONView ex of
         Just v  -> matchM v json
         Nothing -> fail "JSON encoding not supported by exercise"
   f False ex json =
      case json of
         String s -> either fail return (parser ex s)
         _ -> fail "Expecting a string when reading a term" -}