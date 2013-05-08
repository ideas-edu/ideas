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
module Ideas.Service.ModeXML
   ( processXML, xmlRequest, openMathConverter, stringFormatConverter, runEval
   , resultOk, resultError, addVersion
   ) where

import Ideas.Common.Library hiding (exerciseId, (:=))
import Ideas.Common.Utils (Some(..), readM)
import Control.Monad
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Ideas.Service.DomainReasoner
import Ideas.Service.Evaluator
import qualified Ideas.Service.FeedbackText as FeedbackText
import Ideas.Service.FeedbackScript.Syntax
import Ideas.Service.OpenMathSupport
import Ideas.Service.Request
import Ideas.Service.RulesInfo (rulesInfoXML)
import Ideas.Service.State
import Ideas.Service.StrategyInfo
import Ideas.Service.Types
import System.Random
import Ideas.Text.OpenMath.Object
import Ideas.Text.XML
import qualified Ideas.Service.Types as Tp

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
   Some ex  <-
      case exerciseId request of
         Just code -> findExercise code
         Nothing
            | service request == "exerciselist" ->
                 return (Some emptyExercise)
            | otherwise ->
                 fail "unknown exercise code"
   case encoding request of
      Just StringEncoding -> do
         conv <- return (stringFormatConverter ex)
         res  <- runEval (evalService conv srv) xml
         return (resultOk res) 

      _ -> do
         conv <- return (openMathConverter True ex)
         res  <- runEval (evalService conv srv) xml
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

keep :: (XML -> EvalXML a) -> EvalXML a
keep f = get >>= f

runEval :: EvalXML a -> XML -> DomainReasoner a
runEval = evalStateT

stringFormatConverter :: Exercise a -> Evaluator (Const a) EvalXML XMLBuilder
stringFormatConverter ex =
   Evaluator (xmlEncoder False f ex) (xmlDecoder False g ex)
 where
   f  = return . element "expr" . text . prettyPrinter ex
   g = do
      xml0 <- get
      xml  <- findChild "expr" xml0 -- quick fix
      -- guard (name xml == "expr")
      let input = getData xml
      either (fail . show) return (parser ex input)

openMathConverter :: Bool -> Exercise a -> Evaluator (Const a) EvalXML XMLBuilder
openMathConverter withMF ex =
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

xmlEncoder :: Monad m => Bool -> (a -> m XMLBuilder) -> Exercise a -> Encoder (Type a) m XMLBuilder
xmlEncoder isOM enc ex tv@(val ::: tp) =
   case tp of
      -- meta-information
      Tag "Info" (Iso iso t) -> 
         let this = to iso val in
         case t of 
            List (Const Rule) -> return (rulesShortInfo this)
            _ -> rec (this ::: t)
      -- special case for onefirst service; insert elem Tag
      Const String :|: Pair t (Const State) | isJust (equal stepInfoType t) ->
         rec (val ::: (Const String :|: elemType (Pair t (Const State))))
      -- special case for exceptions
      Const String :|: t -> 
         case val of
            Left s  -> fail s
            Right a -> rec (a ::: t)
      -- special cases for lists
      List t@(Const Rule) -> do
         xs <- mapM (\a -> rec (a ::: t)) val
         return (encodeAsList xs)
      List t@(Pair _ _) -> do
         xs <- mapM (\a -> rec (a ::: t)) val
         return (encodeAsList xs)
      List t@(Iso _ _) -> do
         xs <- mapM (\a -> rec (a ::: t)) val
         return (encodeAsList xs)
      --
      Pair t1 t2 -> liftM2 mappend
                       (rec (fst val ::: t1))
                       (rec (snd val ::: t2))
      _ -> encodeWith (xmlEncoderMap isOM ex enc) (xmlEncoderConst isOM enc ex) tv
 where
   rec = xmlEncoder isOM enc ex

encodeAsList :: [XMLBuilder] -> XMLBuilder
encodeAsList = element "list" . mapM_ (element "elem")

xmlEncoderMap :: Monad m => Bool -> Exercise a -> (a -> m XMLBuilder) -> EncoderMap (Const a) m XMLBuilder
xmlEncoderMap isOM ex enc = M.fromList $
   [ ("RulesInfo", \_ -> rulesInfoXML ex enc)
   , ("message", \(val ::: tp) -> do
         f <- equalM (Tag "message" tp) (typed :: Type a FeedbackText.Message)
         let msg = f val
         txt <- encodeText enc ex (FeedbackText.text msg)
         return $ element "message" $ do
            case FeedbackText.accept msg of
               Just b  -> "accept" .=. showBool b
               Nothing -> return ()
            txt)
   , ("Exception", \(val ::: tp) -> do
        f <- equalM tp stringType
        fail (f val))
     -- both element and attribute, depending on context
   , ("LocationId", \(val ::: tp) -> do
        f <- equalM tp (Const Id)
        return $ element "location"$ text $ show $ f val)
   , ("buggy", \tv@(val ::: tp) -> 
        case useAttribute tp of
           Just f -> return ("buggy" .=. f val)
           _ -> liftM (element "buggy") (xmlEncoder isOM enc ex tv))
   ] ++
   -- extra elements
   [ (s, liftM (element s) . xmlEncoder isOM enc ex)
   | s <- [ "list", "elem", "state", "prefix"
          , "similar", "notequiv", "expected", "detour"
          , "correct", "incorrect"]
   ] ++
   -- extra attributes
   [ (s, \(val ::: tp) -> do 
        f <- useAttribute tp
        return (s .=. f val))
   | s <- [ "name", "arguments", "rewriterule", "accept"
          , "exerciseid", "description", "status", "ready", "ruleid"
          , "ruletext", "equivalent"]
   ]

xmlEncoderConst :: Monad m => Bool -> (a -> m XMLBuilder) -> Exercise a -> Encoder (Const a) m XMLBuilder
xmlEncoderConst isOM enc ex (val ::: tp) =
   case tp of
      Exercise  -> return (return ())
      Strategy  -> return (builder (strategyToXML val))
      Rule      -> return ("ruleid" .=. show val) -- encodeRule val)
      State     -> encodeState isOM enc val
      Context   -> encodeContext isOM enc val
      -- Special case for derivationtext
      Derivation (Const String) t -> 
         xmlEncoderConst isOM enc ex (val ::: Derivation (Tag "ruletext" (Const String)) t)     
      Derivation t1 t2 ->
         let xs = map (\(_, s, a) -> (s, a)) (triples val)
         in xmlEncoder isOM enc ex (xs ::: listType (Pair t1 t2))
      Location  -> return ("location" .=. show val)
      Environment -> return (mapM_ (encodeTypedBinding isOM) (bindings val))
      Text      -> encodeText enc ex val
      Bool      -> return (text (showBool val))
      Int       -> return (text (show val))
      String    -> return (text val)
      _         -> fail $ "Type " ++ show tp ++ " not supported in XML"

xmlDecoder :: Bool -> EvalXML a -> Exercise a -> Decoder (Type a) EvalXML
xmlDecoder b f ex = Decoder (xmlDecodeType b ex f)

xmlDecodeType :: Bool -> Exercise a -> EvalXML a -> Type a t -> EvalXML t
xmlDecodeType b ex getTerm serviceType =
   case serviceType of
      Tp.Tag s t
         | s == "answer" -> do
              xml <- get
              c   <- findChild "answer" xml
              put c
              a <- xmlDecodeType b ex getTerm t
              put xml
              return a
         | s == "difficulty" -> keep $ \xml -> do
              g <- equalM difficultyType serviceType
              a <- findAttribute "difficulty" xml
              maybe (fail "unknown difficulty level") (return . g) (readDifficulty a)
         | otherwise -> do
              xml <- get
              cx  <- findChild s xml
              put cx
              a   <- xmlDecodeType b ex getTerm t
              put xml
              return a
      Iso p t  -> liftM (from p) (xmlDecodeType b ex getTerm t)
      Pair t1 t2 -> do
         x <- xmlDecodeType b ex getTerm t1
         y <- xmlDecodeType b ex getTerm t2
         return (x, y)
      t1 :|: t2 ->
         liftM Left  (xmlDecodeType b ex getTerm t1) `mplus`
         liftM Right (xmlDecodeType b ex getTerm t2)
      Unit -> return ()
      Const ctp -> 
         case ctp of
            State    -> decodeState b ex getTerm
            Context  -> decodeContext b ex getTerm
            Rule     -> keep $ fromMaybe (fail "unknown rule") 
                             . liftM (getRule ex . newId . getData) 
                             . findChild "ruleid"
            Environment -> keep $ decodeArgEnvironment b
            Location -> keep $ liftM (toLocation . read . getData) . findChild "location"
            StratCfg -> keep decodeConfiguration
            Script   -> keep $ \xml -> lift $
                           case findAttribute "script" xml of
                              Just s  -> readScript s
                              Nothing -> defaultScript (getId ex)
            StdGen   -> liftIO newStdGen
            Exercise -> return ex
            Id       -> keep $ \xml -> do
                           a <- findChild "location" xml
                           return (newId (getData a))
            _        -> fail $ "No support for argument type in XML: " ++ show serviceType

useAttribute :: Monad m => Type a t -> m (t -> String)
useAttribute (Const String) = return id
useAttribute (Const Bool)   = return showBool
useAttribute (Const Int)    = return show
useAttribute _              = fail "not a primitive type"

encodeState :: Monad m => Bool -> (a -> m XMLBuilder) -> State a -> m XMLBuilder
encodeState isOM enc st = do
   ctx <- encodeContext isOM enc (stateContext st)
   return $ element "state" $ do
      mapM_ (element "prefix" . text . show) (statePrefixes st)
      ctx

rulesShortInfo :: [Rule a] -> XMLBuilder
rulesShortInfo = encodeAsList . map ruleShortInfo

ruleShortInfo :: Rule a -> XMLBuilder
ruleShortInfo r = do 
   "name"        .=. showId r
   "buggy"       .=. showBool (isBuggy r)
   "arguments"   .=. show (length (getRefs r))
   "rewriterule" .=. showBool (isRewriteRule r)

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
      Just this -> liftM makeStrategyConfiguration $ 
                      mapM decodeAction (children this)
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
   loc    = fromLocation (location ctx)
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
   
showBool :: Bool -> String
showBool = map toLower . show