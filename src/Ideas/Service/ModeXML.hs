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
import Control.Monad.State (StateT, evalStateT, get, put, gets)
import Control.Monad.Error
import Data.Char
import Data.List
import Data.Maybe
import Ideas.Service.DomainReasoner
import Ideas.Service.Evaluator
import Ideas.Service.OpenMathSupport
import Ideas.Service.Request
import Ideas.Service.State
import Ideas.Service.EncoderXML
import Ideas.Service.LinkManager
import Ideas.Service.Types
import Ideas.Service.EncoderHTML
import System.Random
import System.IO.Error
import Ideas.Text.OpenMath.Object
import Ideas.Text.XML
import Ideas.Text.HTML
import qualified Ideas.Service.Types as Tp

processXML :: DomainReasoner -> String -> IO (Request, String, String)
processXML dr input = do
   xml  <- either fail return (parseXML input)
   req  <- either fail return (xmlRequest xml)
   resp <- xmlReply dr req xml
              `catchError` (return . resultError . ioeGetErrorString)
   case encoding req of
      Just HTMLEncoding -> 
           let out = show resp
           in return (req, out, "text/html") 
      _ -> let out = showXML (addVersion (version dr) resp)
           in return (req, out, "application/xml")

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

xmlReply :: DomainReasoner -> Request -> XML -> IO XML
xmlReply dr request xml = do
   srv <- findService dr (newId (service request))
   Some ex  <-
      case exerciseId request of
         Just code -> findExercise dr code
         Nothing
            | service request `elem` ["exerciselist", "servicelist", "serviceinfo", "index"] ->
                 return (Some emptyExercise)
            | otherwise ->
                 fail "unknown exercise code"
   case encoding request of
      Just StringEncoding -> do
         conv <- return (stringFormatConverter ex)
         res  <- runEval dr (evalService conv srv) xml
         return (resultOk res) 

      Just HTMLEncoding -> do
         let conv = htmlConverter dr ex
         res  <- runEval dr (evalService conv srv) xml
         return res 

      _ -> do
         conv <- return (openMathConverter True ex)
         res  <- runEval dr (evalService conv srv) xml
         return (resultOk res)  

extractExerciseId :: Monad m => XML -> m Id
extractExerciseId = liftM newId . findAttribute "exerciseid"

resultOk :: XMLBuilder -> XML
resultOk body = either resultError id $ 
   buildXML "reply" $ do
      "result" .=. "ok"
      body

resultError :: String -> XML
resultError txt = makeXML "reply" $ do
   "result" .=. "error"
   element "message" (text txt)

------------------------------------------------------------
-- Mixing abstract syntax (OpenMath format) and concrete syntax (string)

type EvalXML = StateT (DomainReasoner, XML) IO

keep :: (XML -> EvalXML a) -> EvalXML a
keep f = gets snd >>= f

runEval :: DomainReasoner -> EvalXML a -> XML -> IO a
runEval dr m xml = evalStateT m (dr, xml)

stringFormatConverter :: Exercise a -> Evaluator (Const a) EvalXML XMLBuilder
stringFormatConverter ex =
   Evaluator (return . xmlEncoder False f ex) (xmlDecoder False g ex)
 where
   f  = element "expr" . text . prettyPrinter ex
   g = do
      xml0 <- gets snd
      xml  <- findChild "expr" xml0 -- quick fix
      -- guard (name xml == "expr")
      let input = getData xml
      either (fail . show) return (parser ex input)

htmlConverter :: DomainReasoner -> Exercise a -> Evaluator (Const a) EvalXML HTML
htmlConverter dr ex = Evaluator 
   { decoder = decoder (stringFormatConverter ex)
     -- perhaps move link manager to html converter?
   , encoder = return . htmlEncoder (dynamicLinks "ideas.cgi") dr ex
   }

openMathConverter :: Bool -> Exercise a -> Evaluator (Const a) EvalXML XMLBuilder
openMathConverter withMF ex =
   Evaluator (return . xmlEncoder True f ex) (xmlDecoder True g ex)
 where
   f a = toOpenMath ex a >>= builder . toXML . handleMixedFractions
   g = do
      xml   <- gets snd
      xob   <- findChild "OMOBJ" xml
      case xml2omobj xob of
         Left  msg   -> fail msg
         Right omobj -> 
            case fromOpenMath ex omobj of
              Just a  -> return a
              Nothing -> fail "Invalid OpenMath object for this exercise"
   -- Remove special mixed-fraction symbol (depending on boolean argument)
   handleMixedFractions = if withMF then id else noMixedFractions

xmlDecoder :: Bool -> EvalXML a -> Exercise a -> Decoder (Type a) EvalXML
xmlDecoder b f ex = Decoder (xmlDecodeType b ex f)

xmlDecodeType :: Bool -> Exercise a -> EvalXML a -> Type a t -> EvalXML t
xmlDecodeType b ex getTerm serviceType =
   case serviceType of
      Tp.Tag s t
         | s == "answer" -> do
              (dr, xml) <- get
              c   <- findChild "answer" xml
              put (dr, c)
              a <- xmlDecodeType b ex getTerm t
              put (dr, xml)
              return a
         | s == "difficulty" -> keep $ \xml -> do
              g <- equalM difficultyType serviceType
              a <- findAttribute "difficulty" xml
              maybe (fail "unknown difficulty level") (return . g) (readDifficulty a)
         | otherwise -> do
              (dr, xml) <- get
              cx  <- findChild s xml
              put (dr, cx)
              a   <- xmlDecodeType b ex getTerm t
              put (dr, xml)
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
            Script   -> keep $ \xml -> do
                           dr <- gets fst
                           lift $ case findAttribute "script" xml of
                              Just s  -> readScript dr s
                              Nothing -> defaultScript dr (getId ex)
            StdGen   -> liftIO newStdGen
            Exercise -> return ex
            Id       -> keep $ \xml -> do -- improve!
                           a <- findChild "location" xml
                           return (newId (getData a))
            _        -> fail $ "No support for argument type in XML: " ++ show serviceType
      _ -> fail $ "No support for argument type in XML: " ++ show serviceType

decodeState :: Bool -> Exercise a -> EvalXML a -> EvalXML (State a)
decodeState b ex f = do
   (dr, xmlTop) <- get
   xml  <- findChild "state" xmlTop
   put (dr, xml)
   mpr  <- decodePrefix ex xml
   term <- decodeContext b ex f
   put (dr, xmlTop)
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
   xml <- gets snd
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
