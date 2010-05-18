{-# OPTIONS -XGADTs #-}
-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Services using XML notation
--
-----------------------------------------------------------------------------
module Service.ModeXML 
   ( processXML, xmlRequest, openMathConverterTp, stringFormatConverterTp
   , resultOk, resultError, addVersion
   ) where

import Common.Navigator
import Common.Context
import Common.Exercise
import Common.Strategy hiding (not, fail)
import Common.Utils (Some(..), readM)
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Service.ExercisePackage
import Service.ProblemDecomposition
import Service.Request
import Service.RulesInfo (rulesInfoXML)
import Service.StrategyInfo
import Service.TypedAbstractService hiding (exercise)
import Service.Diagnose
import Service.Types
import qualified Service.Types as Tp
import Service.Evaluator
import Text.OpenMath.Object
import Text.XML
import qualified Common.Transformation as Rule
import Service.DomainReasoner

processXML :: String -> DomainReasoner (Request, String, String)
processXML input = do
   xml  <- liftEither (parseXML input)
   req  <- liftEither (xmlRequest xml)
   resp <- xmlReply req xml
              `catchError` \msg -> return (resultError msg)
   vers <- getVersion
   let out = showXML (if null vers then resp else addVersion vers resp)
   return (req, out, "application/xml")

addVersion :: String -> XML -> XML
addVersion s xml = 
   let info = [ "version" := s ]
   in xml { attributes = attributes xml ++ info }

xmlRequest :: XML -> Either String Request
xmlRequest xml = do   
   unless (name xml == "request") $
      fail "expected xml tag request"
   srv  <- findAttribute "service" xml
   let code = extractExerciseCode xml
   enc  <- case findAttribute "encoding" xml of
              Just s  -> liftM Just (readEncoding s)
              Nothing -> return Nothing 
   return Request 
      { service    = srv
      , exerciseID = code
      , source     = findAttribute "source" xml
      , dataformat = XML
      , encoding   = enc
      }

xmlReply :: Request -> XML -> DomainReasoner XML
xmlReply request xml 
   | service request == "mathdox" = do
        code <- maybe (fail "unknown exercise code") return (exerciseID request)
        Some pkg <- findPackage code
        (st, sloc, answer) <- liftEither $ xmlToRequest xml pkg
        return (replyToXML (toOpenMath pkg) (problemDecomposition st sloc answer))

xmlReply request xml = do
   srv <- findService (service request)
   pkg <- 
      case exerciseID request of
         Just code -> findPackage code
         Nothing   
            | service request == "exerciselist" ->
                 return (Some (package emptyExercise))
            | otherwise -> 
                 fail "unknown exercise code"
   Some conv <-
      case encoding request of
         Just StringEncoding -> return (stringFormatConverter pkg)
         _                   -> return (openMathConverter pkg)
   res <- evalService conv srv xml
   return (resultOk res)

extractExerciseCode :: Monad m => XML -> m ExerciseCode
extractExerciseCode xml =
   case liftM (break (== '.')) (findAttribute "exerciseid" xml) of
      Just (as, _:bs) -> return (makeCode as bs)
      Just (as, _)    -> maybe (fail "invalid code") return (readCode as)
      -- being backwards compatible with early MathDox
      Nothing -> do
         let getName = map toLower . filter isAlphaNum . getData
             linalg  = return . makeCode "linalg"
         case fmap getName (findChild "strategy" xml) of
            Just name
               | name == "gaussianelimination"         -> linalg "gaussianelim"
               | name == "gramschmidt"                 -> linalg "gramschmidt"
               | name == "solvelinearsystem"           -> linalg "linsystem"
               | name == "solvelinearsystemwithmatrix" -> linalg "systemwithmatrix"
            _ -> fail "no exerciseid attribute, nor a known strategy element" 

resultOk :: XMLBuilder -> XML
resultOk body = makeXML "reply" $ do 
   "result" .=. "ok"
   body

resultError :: String -> XML
resultError txt = makeXML "reply" $ do 
   "result" .=. "error"
   element "message" (text txt)

------------------------------------------------------------
-- Mixing abstract syntax (OpenMath format) and concrete syntax (string)

stringFormatConverter :: Some ExercisePackage -> Some (Evaluator XML XMLBuilder)
stringFormatConverter (Some pkg) = Some (stringFormatConverterTp pkg)

stringFormatConverterTp :: ExercisePackage a -> Evaluator XML XMLBuilder a
stringFormatConverterTp pkg = 
   Evaluator (xmlEncoder False f ex) (xmlDecoder False g pkg)
 where
   ex = exercise pkg
   f  = return . element "expr" . text . prettyPrinter ex
   g xml = do
      xml <- findChild "expr" xml -- quick fix
      -- guard (name xml == "expr")
      let input = getData xml
      either (fail . show) return (parser ex input)

openMathConverter :: Some ExercisePackage -> Some (Evaluator XML XMLBuilder)
openMathConverter (Some pkg) = Some (openMathConverterTp pkg)
        
openMathConverterTp :: ExercisePackage a -> Evaluator XML XMLBuilder a
openMathConverterTp pkg =
   Evaluator (xmlEncoder True f ex) (xmlDecoder True g pkg)
 where
   ex = exercise pkg
   f = return . builder . toXML . toOpenMath pkg
   g xml = do
      xob   <- findChild "OMOBJ" xml
      omobj <- liftEither (xml2omobj xob)
      case fromOpenMath pkg omobj of
         Just a  -> return a
         Nothing -> fail "Unknown OpenMath object"

xmlEncoder :: Bool -> (a -> DomainReasoner XMLBuilder) -> Exercise a -> Encoder XMLBuilder a
xmlEncoder b f ex = Encoder
   { encodeType  = encode (xmlEncoder b f ex) ex
   , encodeTerm  = f
   , encodeTuple = sequence_
   }
 where
   encode :: Encoder XMLBuilder a -> Exercise a -> Type a t -> t -> DomainReasoner XMLBuilder
   encode enc ex serviceType =
      case serviceType of
         Tp.Tag s _ 
            | s == "Diagnosis" -> \a -> do
                 d <- isSynonym diagnosisTypeSynonym (a ::: serviceType)
                 encodeDiagnosis b (encodeTerm enc) d
            | s == "RulesInfo" -> \_ ->
                 rulesInfoXML ex (encodeTerm enc)
            | s == "state" -> \a -> do
                 st <- isSynonym stateTypeSynonym (a ::: serviceType)
                 encodeState b (encodeTerm enc) st
         Tp.List t1  -> \xs -> 
            case allAreTagged t1 of
               Just f -> do
                  let make = element "elem" . mapM_ (uncurry (.=.)) . f
                  let elems = mapM_ make xs
                  return (element "list" elems)
               _ -> do
                  bs <- mapM (encode enc ex t1) xs
                  let elems = mapM_ (element "elem") bs
                  return (element "list" elems)
         Tp.Tag s t1  -> liftM (element s) . encode enc ex t1  -- quick fix
         Tp.Strategy  -> return . builder . strategyToXML
         Tp.Rule      -> return . ("ruleid" .=.) . Rule.name
         Tp.Term      -> encodeTerm enc
         Tp.Context   -> encodeContext b (encodeTerm enc)
         Tp.Location  -> return . {-element "location" .-} text . show
         Tp.Bool      -> return . text . map toLower . show
         Tp.String    -> return . text
         Tp.Int       -> return . text . show
         _            -> encodeDefault enc serviceType

xmlDecoder :: Bool -> (XML -> DomainReasoner a) -> ExercisePackage a -> Decoder XML a
xmlDecoder b f pkg = Decoder
   { decodeType     = decode (xmlDecoder b f pkg)
   , decodeTerm     = f
   , decoderPackage = pkg
   }
 where
   decode :: Decoder XML a -> Type a t -> XML -> DomainReasoner (t, XML)
   decode dec serviceType = 
      case serviceType of
         Tp.Context     -> leave $ decodeContext b (decoderPackage dec) (decodeTerm dec)
         Tp.Location    -> leave $ liftM (read . getData) . findChild "location"
         Tp.Rule        -> leave $ fromMaybe (fail "unknown rule") . liftM (getRule (decoderExercise dec) . getData) . findChild "ruleid"
         Tp.Term        -> leave $ decodeTerm dec
         Tp.StrategyCfg -> decodeConfiguration
         Tp.Tag s _   
            | s == "state" -> \xml -> do 
                 f  <- equalM Tp.stateTp serviceType
                 st <- decodeState b (decoderPackage dec) (decodeTerm dec) xml
                 return (f st, xml)
         _ -> decodeDefault dec serviceType
         
   leave :: Monad m => (XML -> m a) -> XML -> m (a, XML)
   leave f xml = liftM (\a -> (a, xml)) (f xml)
         
allAreTagged :: Type a t -> Maybe (t -> [(String, String)])
allAreTagged (Iso _ f t) = fmap (. f) (allAreTagged t)
allAreTagged (Pair t1 t2) = do 
   f1 <- allAreTagged t1
   f2 <- allAreTagged t2
   return $ \(a,b) -> f1 a ++ f2 b
allAreTagged (Tag tag Bool)   = Just $ \b -> [(tag, map toLower (show b))]
allAreTagged (Tag tag String) = Just $ \s -> [(tag, s)]
allAreTagged _ = Nothing
         
decodeState :: Monad m => Bool -> ExercisePackage a -> (XML -> m a) -> XML -> m (State a)
decodeState b pkg f top = do
   xml <- findChild "state" top
   unless (name xml == "state") (fail "expected a state tag")
   mpr  <- decodePrefix pkg xml
   term <- decodeContext b pkg f xml
   return (State pkg mpr term)

decodePrefix :: Monad m => ExercisePackage a -> XML -> m (Maybe (Prefix (Context a)))
decodePrefix pkg xml
   | all isSpace prefixText =
        return (Just (emptyPrefix str))
   | prefixText ~= "no prefix" =
        return Nothing 
   | otherwise = do
        a  <- readM prefixText
        pr <- makePrefix a str
        return (Just pr)
 where
   prefixText = maybe "" getData (findChild "prefix" xml)
   str = strategy (exercise pkg)
   a ~= b = g a == g b
   g = map toLower . filter (not . isSpace)
   
decodeContext :: Monad m => Bool -> ExercisePackage a -> (XML -> m a) -> XML -> m (Context a)
decodeContext b pkg f xml = do
   expr <- f xml
   env  <- decodeEnvironment b xml
   return (makeContext (exercise pkg) env expr)

decodeEnvironment :: Monad m => Bool -> XML -> m Environment
decodeEnvironment b xml =
   case findChild "context" xml of 
      Just this -> foldM add emptyEnv (children this)
      Nothing   -> return emptyEnv
 where
   add env item = do 
      unless (name item == "item") $ 
         fail $ "expecting item tag, found " ++ name item
      name  <- findAttribute "name"  item
      case findChild "OMOBJ" item of
         -- OpenMath object found inside item tag
         Just this | b -> do
            case xml2omobj this >>= omobjToTerm of
               Left err -> fail err
               Right term -> 
                  return (storeEnv name term env)
         -- Simple value in attribute
         _ -> do
            value <- findAttribute "value" item
            return (storeEnv name value env)

decodeConfiguration :: MonadPlus m => XML -> m (StrategyConfiguration, XML)
decodeConfiguration xml =
   case findChild "configuration" xml of
      Just this -> mapM decodeAction (children this) >>= \xs -> return (xs, xml)
      Nothing   -> fail "no strategy configuration" 
 where
   decodeAction item = do 
      guard (null (children item))
      action <- 
         case find (\a -> map toLower (show a) == name item) configActions of
            Just a  -> return a
            Nothing -> fail $ "unknown action " ++ show (name item)
      cfgloc <- findAttribute "name" item
      return $ (ByName cfgloc, action)


encodeState :: Monad m => Bool -> (a -> m XMLBuilder) -> State a -> m XMLBuilder
encodeState b f state = f (term state) >>= \body -> return $
   element "state" $ do
      element "prefix"  (text $ maybe "no prefix" show (prefix state))
      let env = getEnvironment (context state)
      encodeEnvironment b (location (context state)) env
      body

encodeEnvironment :: Bool -> Location -> Environment -> XMLBuilder
encodeEnvironment b loc env0
   | nullEnv env = return ()
   | otherwise   = element "context" $ do
        forM_ (keysEnv env) $ \k -> do
           element "item" $ do 
              "name"  .=. k
              case lookupEnv k env of 
                 Just term | b -> builder  (omobj2xml (termToOMOBJ term))
                 _             -> "value" .=. fromMaybe "" (lookupEnv k env)
 where
   env | null loc  = env0
       | otherwise = storeEnv "location" loc env0

encodeDiagnosis :: Monad m => Bool -> (a -> m XMLBuilder) -> Diagnosis a -> m XMLBuilder
encodeDiagnosis mode f diagnosis =
   case diagnosis of
      Buggy r        -> return $ element "buggy" $ "ruleid" .=. Rule.name r
      NotEquivalent  -> return $ tag "notequiv"
      Similar  b s   -> ok "similar"  b s Nothing
      Expected b s r -> ok "expected" b s (Just r)
      Detour   b s r -> ok "detour"   b s (Just r)
      Correct  b s   -> ok "correct"  b s Nothing
 where
   ok t b s mr = do
      body <- encodeState mode f s
      return $ element t $ do
         "ready" .=. map toLower (show b)
         maybe (return ()) (("ruleid" .=.) . Rule.name) mr
         body
  
encodeContext :: Monad m => Bool -> (a -> m XMLBuilder) -> Context a -> m XMLBuilder
encodeContext b f ctx = do
   a   <- fromContext ctx
   xml <- f a
   return (xml >> encodeEnvironment b (location ctx) (getEnvironment ctx))