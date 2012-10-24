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
-- Services using XML notation
--
-----------------------------------------------------------------------------
module Service.ModeXML
   ( processXML, xmlRequest, openMathConverterTp, stringFormatConverterTp, runEval
   , resultOk, resultError, addVersion
   ) where

import Common.Library hiding (exerciseId, (:=))
import Common.Utils (Some(..), readM)
import Control.Monad
import Control.Monad.State (StateT, evalStateT, get, put, modify)
import Control.Monad.Trans
import Data.Char
import Data.List
import Data.Maybe
import Service.DomainReasoner
import Service.Evaluator
import Service.FeedbackScript.Syntax
import Service.OpenMathSupport
import Service.Request
import Service.RulesInfo (rulesInfoXML)
import Service.State
import Service.StrategyInfo
import Service.Types
import Text.OpenMath.Object
import Text.HTML hiding (text)
import Text.XML
import qualified Service.Types as Tp

processXML :: String -> DomainReasoner (Request, String, String)
processXML input = do
   xml  <- liftEither (parseXML input)
   req  <- liftEither (xmlRequest xml)
   resp <- xmlReply req xml
              `catchError` (return . resultError)
   vers <- getVersion
   let out | encoding req == Just HTMLEncoding = show resp
           | otherwise = showXML (if null vers then resp else addVersion vers resp)
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
   ex  <-
      case exerciseId request of
         Just code -> findExercise code
         Nothing
            | service request == "exerciselist" ->
                 return (Some emptyExercise)
            | otherwise ->
                 fail "unknown exercise code"
   case encoding request of
      Just StringEncoding -> do
         Some conv <- return (stringFormatConverter ex)
         res <- runEval (evalService conv srv) xml
         return (resultOk res) 
         
      Just HTMLEncoding -> do
         Some conv <- return (htmlEvaluator ex)
         res <- runEval (evalService conv srv) xml
         return (htmlPage "Title" Nothing res) 
      _ -> do 
         Some conv <- return (openMathConverter ex)
         res <- runEval (evalService conv srv) xml
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

type EvalXML = StateT XML DomainReasoner

runEval :: EvalXML a -> XML -> DomainReasoner a
runEval = evalStateT

stringFormatConverter :: Some Exercise -> Some (Evaluator EvalXML XMLBuilder)
stringFormatConverter (Some ex) = Some (stringFormatConverterTp ex)

stringFormatConverterTp :: Exercise a -> Evaluator EvalXML XMLBuilder a
stringFormatConverterTp ex =
   Evaluator (xmlEncoder False f ex) (xmlDecoder False g ex)
 where
   f  = return . element "expr" . text . prettyPrinter ex
   g = do
      xml0 <- get
      xml  <- findChild "expr" xml0 -- quick fix
      -- guard (name xml == "expr")
      let input = getData xml
      either (fail . show) return (parser ex input)

openMathConverter :: Some Exercise -> Some (Evaluator EvalXML XMLBuilder)
openMathConverter (Some ex) = Some (openMathConverterTp True ex)

openMathConverterTp :: Bool -> Exercise a -> Evaluator EvalXML XMLBuilder a
openMathConverterTp withMF ex =
   Evaluator (xmlEncoder True f ex) (xmlDecoder True g ex)
 where
   f a = liftM (builder . toXML) $ handleMixedFractions $ toOpenMath ex a
   g = do
      xml   <- get 
      xob   <- findChild "OMOBJ" xml
      case xml2omobj xob of
         Left  msg   -> fail msg
         Right omobj -> 
            case fromOpenMath ex omobj of
              Just a  -> return a
              Nothing -> fail "Invalid OpenMath object for this exercise"
   -- Remove special mixed-fraction symbol (depending on boolean argument)
   handleMixedFractions = if withMF then id else liftM noMixedFractions

xmlEncoder :: Bool -> (a -> EvalXML XMLBuilder) -> Exercise a -> Encoder EvalXML XMLBuilder a
xmlEncoder isOM enc ex (val ::: tp) =
   case tp of
      Iso p t    -> xmlEncoder isOM enc ex (to p val ::: t)
      Pair t1 t2 -> do
         sx <- xmlEncoder isOM enc ex (fst val ::: t1)
         sy <- xmlEncoder isOM enc ex (snd val ::: t2)
         return (sx >> sy)
      t1 :|: t2 -> case val of
                      Left  x -> xmlEncoder isOM enc ex (x ::: t1)
                      Right y -> xmlEncoder isOM enc ex (y ::: t2)
       
      List t -> liftM sequence_ (mapM (xmlEncoder isOM enc ex . (::: t)) val)
      Exercise      -> return (return ())
      Exception     -> fail val
      Unit          -> return (return ())
      Id            -> return (text (show val))
      IO t          -> do x <- liftIO (runIO val)
                          xmlEncoder isOM enc ex (x ::: Exception :|: t)
      Tp.Tag s t1
         | s == "RulesInfo" -> 
              rulesInfoXML ex enc
         | otherwise ->
              case useAttribute t1 of
                 Just f | s /= "message" -> return (s .=. f val)
                 _  -> liftM (element s) (xmlEncoder isOM enc ex (val ::: t1))
      Tp.Strategy   -> return (builder (strategyToXML val))
      Tp.Rule       -> return ("ruleid" .=. showId val)
      Tp.Term       -> enc val
      Tp.Context    -> encodeContext isOM enc val
      Tp.Location   -> return ("location" .=. show val)
      Tp.BindingTp  -> return (encodeTypedBinding isOM val)
      Tp.Text       -> encodeText enc ex val
      Tp.Bool       -> return (text (map toLower (show val)))
      Tp.Int        -> return (text (show val))
      Tp.String     -> return (text val)
      _             -> fail $ "Type " ++ show tp ++ " not supported in XML"

xmlDecoder :: Bool -> EvalXML a -> Exercise a -> Decoder EvalXML a
xmlDecoder b f ex = Decoder
   { decodeType      = xmlDecodeType b (xmlDecoder b f ex)
   , decodeTerm      = f
   , decoderExercise = ex
   }

xmlDecodeType :: Bool -> Decoder EvalXML a -> Type a t -> EvalXML t
xmlDecodeType b dec serviceType =
   case serviceType of
      Tp.Context     -> decodeContext b (decoderExercise dec) (decodeTerm dec)
      Tp.Location    -> keep $ liftM (read . getData) . findChild "location"
      Tp.Id          -> keep $ \xml -> do
                           a <- findChild "location" xml
                           return (newId (getData a))
      Tp.Rule        -> keep $ fromMaybe (fail "unknown rule") . liftM (getRule (decoderExercise dec) . newId . getData) . findChild "ruleid"
      Tp.Term        -> decodeTerm dec
      Tp.StrategyCfg -> keep decodeConfiguration
      Tp.Script      -> keep $ \xml ->
                           case findAttribute "script" xml of
                              Just s  -> lift (readScript s)
                              Nothing ->
                                 lift (defaultScript (getId (decoderExercise dec)))
      Tp.Tag s t
         | s == "state" -> do
              g  <- equalM stateType serviceType
              st <- decodeState b (decoderExercise dec) (decodeTerm dec)
              return (g st)
         | s == "answer" -> do
              xml <- get
              c   <- findChild "answer" xml
              put c
              a <- xmlDecodeType b dec t
              put xml
              return a
         | s == "difficulty" -> keep $ \xml -> do
              g <- equalM difficultyType serviceType
              a <- findAttribute "difficulty" xml
              maybe (fail "unknown difficulty level") (return . g) (readDifficulty a)
         {- s == "prefix" -> \xml -> do
              f  <- equalM String t
              mp <- decodePrefix (decoderExercise dec) xml
              s  <- maybe (fail "no prefix") (return . show) mp
              return (f s, xml) -}
         | s == "args" -> keep $ \xml -> do
              g   <- equalM envType t
              env <- decodeArgEnvironment b xml
              return (g env)
         | otherwise -> do
              xml <- get
              cx  <- findChild s xml
              put cx
              a   <- xmlDecodeType b dec t
              put xml
              return a

      Iso p t  -> liftM (from p) (xmlDecodeType b dec t)
      Pair t1 t2 -> do
         a <- xmlDecodeType b dec t1
         b <- xmlDecodeType b dec t2
         return (a, b)
      t1 :|: t2 ->
         liftM Left  (xmlDecodeType b dec t1) `mplus`
         liftM Right (xmlDecodeType b dec t2)
      Unit ->
         return ()
      Tag _ t1 ->
         xmlDecodeType b dec t1
      Exercise ->
         return (decoderExercise dec)
      Script ->
         lift (defaultScript (getId (decoderExercise dec)))
      _ ->
         fail $ "No support for argument type in XML: " ++ show serviceType
 where
   keep :: (XML -> EvalXML a) -> EvalXML a
   keep f = do
      xml <- get
      f xml

useAttribute :: Type a t -> Maybe (t -> String)
useAttribute String = Just id
useAttribute Bool   = Just (map toLower . show)
useAttribute Int    = Just show
useAttribute _      = Nothing

decodeState :: Bool -> Exercise a -> EvalXML a -> EvalXML (State a)
decodeState b ex f = do
   xmlTop <- get
   xml  <- findChild "state" xmlTop
   put xml
   mpr  <- decodePrefix ex xml
   term <- decodeContext b ex f
   put xmlTop
   return (makeState ex mpr term)

decodePrefix :: Exercise a -> XML -> EvalXML [Prefix (Context a)]
decodePrefix ex xml
   | all isSpace prefixText =
        return [emptyPrefix str]
   | prefixText ~= "no prefix" =
        return []
   | otherwise = do
        a  <- readM prefixText
        pr <- makePrefix a str
        return [pr]
 where
   prefixText = maybe "" getData (findChild "prefix" xml)
   str = strategy ex
   a ~= b = g a == g b
   g = map toLower . filter (not . isSpace)

decodeContext :: Bool -> Exercise a -> EvalXML a -> EvalXML (Context a)
decodeContext b ex f = do
   expr <- f
   env  <- decodeEnvironment b
   return (makeContext ex env expr)

decodeEnvironment :: Bool -> EvalXML Environment
decodeEnvironment b = do
   xml <- get
   case findChild "context" xml of
      Just this -> foldM add mempty (children this)
      Nothing   -> return mempty
 where
   add env item = do
      unless (name item == "item") $
         fail $ "expecting item tag, found " ++ name item
      n  <- findAttribute "name"  item
      case findChild "OMOBJ" item of
         -- OpenMath object found inside item tag
         Just this | b ->
            case xml2omobj this >>= fromOMOBJ of
               Left err -> fail err
               Right term ->
                  return $ insertRef (makeRef n) (term :: Term) env
         -- Simple value in attribute
         _ -> do
            value <- findAttribute "value" item
            return $ insertRef (makeRef n) value env

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

encodeEnvironment :: Bool -> Context a -> XMLBuilder
encodeEnvironment b ctx
   | null values = return ()
   | otherwise = element "context" $
        forM_ values $ \tb ->
           element "item" $ do
              "name"  .=. showId tb
              case getTermValue tb of
                 term | b -> 
                    builder (omobj2xml (toOMOBJ term))
                 _ -> "value" .=. showValue tb
 where
   loc    = location ctx
   values = bindings (withLoc ctx)
   withLoc
      | null loc  = id
      | otherwise = insertRef (makeRef "location") loc 

encodeContext :: Monad m => Bool -> (a -> m XMLBuilder) -> Context a -> m XMLBuilder
encodeContext b f ctx = do
   a   <- fromContext ctx
   xml <- f a
   return (xml >> encodeEnvironment b ctx)

encodeTypedBinding :: Bool -> Binding -> XMLBuilder
encodeTypedBinding b tb = element "argument" $ do
   "description" .=. showId tb
   case getTermValue tb of
      term | b -> builder $ 
         omobj2xml $ toOMOBJ term
      _ -> text (showValue tb)

decodeArgEnvironment :: MonadPlus m => Bool -> XML -> m Environment
decodeArgEnvironment b = 
   liftM makeEnvironment . mapM make . filter isArg . children
 where
   isArg = (== "argument") . name
 
   make :: MonadPlus m => XML -> m Binding
   make xml = do
      a <- findAttribute "description" xml
      case findChild "OMOBJ" xml of
         -- OpenMath object found inside tag
         Just this | b -> 
            case xml2omobj this >>= fromOMOBJ of
               Left err   -> fail err
               Right term -> return (termBinding a term)
         -- Simple value
         _ -> return (makeBinding (makeRef a) (getData xml))
         
   termBinding :: String -> Term -> Binding
   termBinding = makeBinding . makeRef
   
encodeText :: Monad m => (a -> m XMLBuilder) -> Exercise a -> Text -> m XMLBuilder
encodeText f ex = liftM sequence_ . mapM make . textItems
 where
   make t@(TextTerm a) = fromMaybe (returnText t) $ do
      v <- hasTermView ex
      b <- match v a
      return (f b)
   make a = returnText a
   
   returnText = return . text . show
   
--------------------

htmlEvaluator :: Some Exercise -> Some (Evaluator EvalXML HTMLBuilder)
htmlEvaluator (Some ex) =
   Some (Evaluator (htmlEncoder f ex) (decoder (stringFormatConverterTp ex)))
 where
   f  = return . preText . prettyPrinter ex

htmlEncoder :: (a -> EvalXML HTMLBuilder) -> Exercise a -> Encoder EvalXML HTMLBuilder a
htmlEncoder enc ex (_ ::: tp) =
   case tp of {-
      Iso p t    -> htmlEncoder enc ex t (to p a)
      Pair t1 t2 -> do
         sx <- htmlEncoder enc ex t1 (fst a)
         sy <- htmlEncoder enc ex t2 (snd a)
         return (sx >> sy)
      t1 :|: t2 -> case a of
                      Left  x -> htmlEncoder enc ex t1 x
                      Right y -> htmlEncoder enc ex t2 y
       
      List t        -> liftM sequence_ (mapM (htmlEncoder enc ex t) a)
      Exercise      -> return (return ())
      Exception     -> fail a
      Unit          -> return (return ())
      Id            -> return (text (show a))
      IO t          -> do x <- liftIO (runIO a)
                          htmlEncoder enc ex (Exception :|: t) x
      Tp.Tag _ t1   -> htmlEncoder enc ex t1 a
      Tp.Strategy   -> return (builder (strategyToXML a))
      Tp.Rule       -> return (preText ("   => " ++ showId a))
      Tp.Term       -> enc a
      Tp.Context    -> encodeContext False enc a
      Tp.Location   -> return ("location" .=. show a)
      Tp.BindingTp  -> return $ preText ("     {" ++ showId a ++ " = " ++ showValue a ++ "}")
      Tp.Text       -> encodeText enc ex a
      Tp.Bool       -> return (text (map toLower (show a)))
      Tp.Int        -> return (text (show a))
      Tp.String     -> return (text a) -}
      _             -> fail $ "Type " ++ show tp ++ " not supported in XML"