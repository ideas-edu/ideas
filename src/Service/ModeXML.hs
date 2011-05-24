{-# LANGUAGE GADTs #-}
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

import Common.Library hiding (exerciseId)
import Common.Utils (Some(..), readM)
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Service.ExercisePackage
import Service.Request
import Service.RulesInfo (rulesInfoXML)
import Service.StrategyInfo
import Service.State
import Service.Types
import qualified Service.Types as Tp
import Service.Evaluator
import Text.OpenMath.Object
import Text.XML
import Service.DomainReasoner

processXML :: String -> DomainReasoner (Request, String, String)
processXML input = do
   xml  <- liftEither (parseXML input)
   req  <- liftEither (xmlRequest xml)
   resp <- xmlReply req xml
              `catchError` (return . resultError)
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
   let a = extractExerciseId xml
   enc  <- case findAttribute "encoding" xml of
              Just s  -> liftM Just (readEncoding s)
              Nothing -> return Nothing 
   return Request 
      { service    = srv
      , exerciseId = a
      , source     = findAttribute "source" xml
      , dataformat = XML
      , encoding   = enc
      }

xmlReply :: Request -> XML -> DomainReasoner XML
xmlReply request xml = do
   srv <- findService (service request)
   pkg <- 
      case exerciseId request of
         Just code -> findExercise code
         Nothing   
            | service request == "exerciselist" ->
                 return (Some emptyExercise)
            | otherwise -> 
                 fail "unknown exercise code"
   Some conv <-
      case encoding request of
         Just StringEncoding -> return (stringFormatConverter pkg)
         _                   -> return (openMathConverter pkg)
   res <- evalService conv srv xml
   return (resultOk res)

extractExerciseId :: Monad m => XML -> m Id
extractExerciseId = liftM newId . findAttribute "exerciseid"

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
   Evaluator (xmlEncoder False f pkg) (xmlDecoder False g pkg)
 where
   ex = pkg
   f  = return . element "expr" . text . prettyPrinter ex
   g xml0 = do
      xml <- findChild "expr" xml0 -- quick fix
      -- guard (name xml == "expr")
      let input = getData xml
      either (fail . show) return (parser ex input)

openMathConverter :: Some ExercisePackage -> Some (Evaluator XML XMLBuilder)
openMathConverter (Some pkg) = Some (openMathConverterTp pkg)
        
openMathConverterTp :: ExercisePackage a -> Evaluator XML XMLBuilder a
openMathConverterTp pkg =
   Evaluator (xmlEncoder True f pkg) (xmlDecoder True g pkg)
 where
   f = liftM (builder . toXML) . toOpenMath pkg
   g xml = do
      xob   <- findChild "OMOBJ" xml
      omobj <- liftEither (xml2omobj xob)
      case fromOpenMath pkg omobj of
         Just a  -> return a
         Nothing -> fail "Invalid OpenMath object for this exercise"

xmlEncoder :: Bool -> (a -> DomainReasoner XMLBuilder) -> ExercisePackage a -> Encoder XMLBuilder a
xmlEncoder b f pkg = Encoder
   { encodeType  = xmlEncodeType b (xmlEncoder b f pkg) pkg
   , encodeTerm  = f
   , encodeTuple = sequence_
   }

xmlEncodeType :: Bool -> Encoder XMLBuilder a -> ExercisePackage a -> Type a t -> t -> DomainReasoner XMLBuilder
xmlEncodeType b enc pkg serviceType =
   case serviceType of
      Tp.Tag s t1
         | s == "RulesInfo" -> \_ ->
              rulesInfoXML pkg (encodeTerm enc)
         | otherwise ->  
              case useAttribute t1 of
                 Just f | s /= "message" -> return . (s .=.) . f
                 _  -> liftM (element s) . xmlEncodeType b enc pkg t1
      Tp.Strategy   -> return . builder . strategyToXML
      Tp.Rule       -> return . ("ruleid" .=.) . showId
      Tp.Term       -> encodeTerm enc
      Tp.Context    -> encodeContext b (encodeTerm enc)
      Tp.Location   -> return . ("location" .=.) . show
      Tp.ArgValueTp -> return . encodeArgValue b
      Tp.Bool       -> return . text . map toLower . show
      Tp.String     -> return . text
      _             -> encodeDefault enc serviceType

xmlDecoder :: Bool -> (XML -> DomainReasoner a) -> ExercisePackage a -> Decoder XML a
xmlDecoder b f pkg = Decoder
   { decodeType      = xmlDecodeType b (xmlDecoder b f pkg)
   , decodeTerm      = f
   , decoderExercise = pkg
   }

xmlDecodeType :: Bool -> Decoder XML a -> Type a t -> XML -> DomainReasoner (t, XML)
xmlDecodeType b dec serviceType =
   case serviceType of
      Tp.Context     -> keep $ decodeContext b (decoderExercise dec) (decodeTerm dec)
      Tp.Location    -> keep $ liftM (read . getData) . findChild "location"
      Tp.Id          -> keep $ \xml -> do
                           a <- findChild "location" xml
                           return (newId (getData a))
      Tp.Rule        -> keep $ fromMaybe (fail "unknown rule") . liftM (getRule (decoderExercise dec) . newId . getData) . findChild "ruleid"
      Tp.Term        -> keep $ decodeTerm dec
      Tp.StrategyCfg -> keep decodeConfiguration
      Tp.Tag s t
         | s == "state" -> keep $ \xml -> do 
              g  <- equalM stateType serviceType
              st <- decodeState b (decoderExercise dec) (decodeTerm dec) xml
              return (g st)
         | s == "answer" -> keep $ \xml -> do
              c <- findChild "answer" xml 
              (a, _) <- xmlDecodeType b dec t c
              return a
         | s == "difficulty" -> keep $ \xml -> do
              g <- equalM difficultyType serviceType
              a <- findAttribute "difficulty" xml
              maybe (fail "unknown difficulty level") (return . g) (readDifficulty a)
         
         {-
         | s == "script" -> keep $ \xml -> do
              g <- equalM (Tag "script" String) serviceType
              a <- findAttribute "script" xml
              return (g a)
         | s == "prefix" -> \xml -> do
              f  <- equalM String t
              mp <- decodePrefix (decoderPackage dec) xml
              s  <- maybe (fail "no prefix") (return . show) mp
              return (f s, xml) -}
         | otherwise -> keep $ \xml -> do
              c <- findChild s xml
              (a, _) <- xmlDecodeType b dec t c
              return a
              
      _ -> decodeDefault dec serviceType
 where         
   keep :: Monad m => (XML -> m a) -> XML -> m (a, XML)
   keep f xml = liftM (\a -> (a, xml)) (f xml)

useAttribute :: Type a t -> Maybe (t -> String)
useAttribute String = Just id
useAttribute Bool   = Just (map toLower . show)
useAttribute _      = Nothing
         
decodeState :: Monad m => Bool -> ExercisePackage a -> (XML -> m a) -> XML -> m (State a)
decodeState b pkg f xmlTop = do
   xml  <- findChild "state" xmlTop
   mpr  <- decodePrefix pkg xml
   term <- decodeContext b pkg f xml
   return (makeState pkg mpr term)

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
   str = strategy pkg
   a ~= b = g a == g b
   g = map toLower . filter (not . isSpace)
   
decodeContext :: Monad m => Bool -> ExercisePackage a -> (XML -> m a) -> XML -> m (Context a)
decodeContext b pkg f xml = do
   expr <- f xml
   env  <- decodeEnvironment b xml
   return (makeContext pkg env expr)

decodeEnvironment :: Monad m => Bool -> XML -> m Environment
decodeEnvironment b xml =
   case findChild "context" xml of 
      Just this -> foldM add emptyEnv (children this)
      Nothing   -> return emptyEnv
 where
   add env item = do 
      unless (name item == "item") $ 
         fail $ "expecting item tag, found " ++ name item
      n  <- findAttribute "name"  item
      case findChild "OMOBJ" item of
         -- OpenMath object found inside item tag
         Just this | b ->
            case xml2omobj this >>= omobjToTerm of
               Left err -> fail err
               Right term -> 
                  return (storeEnv n term env)
         -- Simple value in attribute
         _ -> do
            value <- findAttribute "value" item
            return (storeEnv n value env)

decodeConfiguration :: MonadPlus m => XML -> m StrategyConfiguration
decodeConfiguration xml =
   case findChild "configuration" xml of
      Just this -> mapM decodeAction (children this)
      Nothing   -> fail "no strategy configuration" 
 where
   decodeAction item = do 
      guard (null (children item))
      action <- 
         case find (\a -> map toLower (show a) == name item) configActions of
            Just a  -> return a
            Nothing -> fail $ "unknown action " ++ show (name item)
      cfgloc <- findAttribute "name" item
      return (byName (newId cfgloc), action)
   
encodeEnvironment :: Bool -> Location -> Environment -> XMLBuilder
encodeEnvironment b loc env0
   | nullEnv env = return ()
   | otherwise   = element "context" $
        forM_ (keysEnv env) $ \k ->
           element "item" $ do 
              "name"  .=. k
              case lookupEnv k env of 
                 Just term | b -> builder  (omobj2xml (termToOMOBJ term))
                 _             -> "value" .=. fromMaybe "" (lookupEnv k env)
 where
   env | null loc  = env0
       | otherwise = storeEnv "location" loc env0

encodeContext :: Monad m => Bool -> (a -> m XMLBuilder) -> Context a -> m XMLBuilder
encodeContext b f ctx = do
   a   <- fromContext ctx
   xml <- f a
   return (xml >> encodeEnvironment b (location ctx) (getEnvironment ctx))

encodeArgValue :: Bool -> ArgValue -> XMLBuilder
encodeArgValue b (ArgValue descr a) = element "argument" $ do
   "description" .=. labelArgument descr
   showValue a
 where
   showValue 
      | b         = builder . omobj2xml . termToOMOBJ . build (termViewArgument descr)
      | otherwise = text . showArgument descr
   