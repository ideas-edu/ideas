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
module Ideas.Service.ModeXML (processXML) where

import Ideas.Common.Library hiding (exerciseId, (:=))
import Ideas.Common.Utils (Some(..), readM)
import Control.Monad
import Control.Monad.Error
import Data.Char
import Data.List
import Ideas.Service.DomainReasoner
import Ideas.Service.Evaluator
import Ideas.Service.OpenMathSupport
import Ideas.Service.Request
import Ideas.Service.State
import Ideas.Service.EncoderXML hiding (getExercise, isOpenMath)
import Ideas.Service.LinkManager
import Ideas.Service.FeedbackScript.Syntax (Script)
import Ideas.Service.Types
import Ideas.Service.EncoderHTML
import Ideas.Service.FeedbackScript.Parser (parseScriptSafe)
import System.Random (StdGen, newStdGen)
import System.IO.Error
import Ideas.Text.OpenMath.Object
import Ideas.Text.XML
import Ideas.Text.HTML
import qualified Ideas.Service.Types as Tp

processXML :: DomainReasoner -> Maybe String -> String -> IO (Request, String, String)
processXML dr cgiBin input = do
   xml  <- either fail return (parseXML input)
   req  <- either fail return (xmlRequest xml)
   resp <- xmlReply dr cgiBin req xml
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

xmlReply :: DomainReasoner -> Maybe String -> Request -> XML -> IO XML
xmlReply dr cgiBin request xml = do
   srv <- findService dr (newId (service request))
   Some ex  <-
      case exerciseId request of
         Just code -> findExercise dr code
         Nothing
            | service request `elem` ["exerciselist", "servicelist", "serviceinfo", "index"] ->
                 return (Some emptyExercise)
            | otherwise ->
                 fail "unknown exercise code"
   script <- case findAttribute "script" xml of
                Just s  -> parseScriptSafe s
                Nothing 
                   | getId ex == mempty -> return mempty
                   | otherwise          -> defaultScript dr (getId ex)
   stdgen <- newStdGen
   case encoding request of
      Just StringEncoding -> do
         res <- evalService (stringFormatConverter script ex stdgen xml) srv
         return (resultOk res) 

      Just HTMLEncoding -> do
         res <- evalService (htmlConverter dr cgiBin script ex stdgen xml) srv
         return res 

      _ -> do
         res <- evalService (openMathConverter True script ex stdgen xml) srv
         return (resultOk res)  

extractExerciseId :: Monad m => XML -> m Id
extractExerciseId = liftM newId . findAttribute "exerciseid"

resultOk :: XMLBuilder -> XML
resultOk body = makeXML "reply" $
   ("result" .=. "ok")
   <> body

resultError :: String -> XML
resultError txt = makeXML "reply" $
   ("result" .=. "error")
   <> tag "message" (string txt)

------------------------------------------------------------
-- Mixing abstract syntax (OpenMath format) and concrete syntax (string)

type XMLDecoder a = EncoderState (XMLDecoderState a) XML

data XMLDecoderState a = XMLDecoderState
   { getExercise       :: Exercise a
   , getScript         :: Script
   , getStdGen         :: StdGen
   , isOpenMath        :: Bool
   , decodeTerm        :: XML -> Either String a
   }

stringFormatConverter :: Script -> Exercise a -> StdGen -> XML -> Evaluator a IO XMLBuilder
stringFormatConverter script ex stdgen xml =
   Evaluator (runEncoderStateM xmlEncoder xes) 
             (\tp -> runEncoderStateM (xmlDecoder tp) xds xml)
 where
   xes = XMLEncoderState ex False (tag "expr" . string . prettyPrinter ex)
   xds = XMLDecoderState ex script stdgen False g
   g xml = liftM getData (findChild "expr" xml) >>= parser ex

htmlConverter :: DomainReasoner -> Maybe String -> Script -> Exercise a -> StdGen -> XML -> Evaluator a IO HTML
htmlConverter dr cgiBin script ex stdgen xml = 
   Evaluator (return . htmlEncoder lm dr ex) d
 where
   lm = maybe staticLinks dynamicLinks cgiBin
   Evaluator _ d = stringFormatConverter script ex stdgen xml

openMathConverter :: Bool -> Script -> Exercise a -> StdGen -> XML -> Evaluator a IO XMLBuilder
openMathConverter withMF script ex stdgen xml =
   Evaluator (runEncoderStateM xmlEncoder xes)
             (\tp -> runEncoderStateM (xmlDecoder tp) xds xml)
 where
   xes = XMLEncoderState ex True h
   xds = XMLDecoderState ex script stdgen True g
   h a = case toOpenMath ex a of
            Left err    -> error "Error encoding term in OpenMath" -- fix me!
            Right omobj -> builder (toXML (handleMixedFractions omobj))
   g xml = do
      xob <- findChild "OMOBJ" xml
      case xml2omobj xob of
         Left  msg   -> Left msg
         Right omobj -> 
            case fromOpenMath ex omobj of
              Just a  -> Right a
              Nothing -> Left "Invalid OpenMath object for this exercise"
   -- Remove special mixed-fraction symbol (depending on boolean argument)
   handleMixedFractions = if withMF then id else noMixedFractions

xmlDecoder :: Type a t -> XMLDecoder a t
xmlDecoder tp =
   case tp of
      Tp.Tag s t
         | s == "answer" -> do
              c <- encoderFor (findChild "answer")
              xmlDecoder t // c 
         | s == "Difficulty" -> do
              g <- equalM typed tp
              a <- encoderFor (findAttribute "difficulty")
              maybe (fail "unknown difficulty level") (return . g) (readDifficulty a)
         | otherwise -> do
              cx <- encoderFor (findChild s)
              xmlDecoder t // cx
      Iso p t  -> liftM (from p) (xmlDecoder t) 
      Pair t1 t2 -> do
         x <- xmlDecoder t1
         y <- xmlDecoder t2
         return (x, y) 
      t1 :|: t2 ->
         liftM Left  (xmlDecoder t1) `mplus`
         liftM Right (xmlDecoder t2)
      Unit -> return ()
      Const ctp -> 
         case ctp of
            State       -> decodeState
            Context     -> decodeContext
            Rule        -> decodeRule
            Environment -> decodeArgEnvironment
            Location    -> decodeLocation
            StratCfg    -> decodeConfiguration
            StdGen      -> withState getStdGen
            Script      -> withState getScript
            Exercise    -> withState getExercise
            Id          -> do -- improve!
                              a <- encoderFor (findChild "location")
                              return (newId (getData a))
            _ -> fail $ "No support for argument type in XML: " ++ show tp
      _ -> fail $ "No support for argument type in XML: " ++ show tp

decodeRule :: XMLDecoder a (Rule (Context a))
decodeRule = do
   ex <- withState getExercise
   xml0 <- encoderFor (findChild "ruleid")
   getRule ex (newId (getData xml0))

decodeLocation :: XMLDecoder a Location
decodeLocation = do
   xml <- encoderFor (findChild "location")
   return (toLocation (read (getData xml)))

decodeState :: XMLDecoder a (State a)
decodeState = do
   ex   <- withState getExercise
   xml  <- encoderFor (findChild "state")
   mpr  <- decodePrefix  // xml
   term <- decodeContext // xml
   return (makeState ex mpr term)

decodePrefix :: XMLDecoder a [Prefix (Context a)]
decodePrefix = do
   str <- liftM strategy (withState getExercise)
   prefixText <- simpleEncoder (maybe "" getData . findChild "prefix")
   if all isSpace prefixText
      then return [emptyPrefix str]
      else if prefixText ~= "no prefix"
      then return []
      else do
         a  <- readM prefixText
         pr <- makePrefix a str
         return [pr]
 where
   a ~= b = g a == g b
   g = map toLower . filter (not . isSpace) 

decodeContext :: XMLDecoder a (Context a)
decodeContext = do
   ex   <- withState getExercise
   f    <- withState decodeTerm
   expr <- encoderFor (either fail return . f)
   env  <- decodeEnvironment
   return (makeContext ex env expr)

decodeEnvironment :: XMLDecoder a Environment
decodeEnvironment = encoderFor $ \xml ->
   case findChild "context" xml of
      Just this -> foldM add mempty (children this)
      Nothing   -> return mempty
 where
   add env item = do
      unless (name item == "item") $
         fail $ "expecting item tag, found " ++ name item
      n    <- findAttribute "name"  item
      isOM <- withState isOpenMath
      case findChild "OMOBJ" item of
         -- OpenMath object found inside item tag
         Just this | isOM ->
            case xml2omobj this >>= fromOMOBJ of
               Left err -> fail err
               Right term ->
                  return $ insertRef (makeRef n) (term :: Term) env
         -- Simple value in attribute
         _ -> do
            value <- findAttribute "value" item
            return $ insertRef (makeRef n) value env

decodeConfiguration :: XMLDecoder a StrategyConfiguration
decodeConfiguration = do
   xml <- encoderFor (findChild "configuration")
   liftM makeStrategyConfiguration $ 
      mapM decodeAction (children xml)
 where
   decodeAction item = do
      guard (null (children item))
      action <-
         case find (\a -> map toLower (show a) == name item) configActions of
            Just a  -> return a
            Nothing -> fail $ "unknown action " ++ show (name item)
      cfgloc <- findAttribute "name" item
      return (byName (newId cfgloc), action)

decodeArgEnvironment :: XMLDecoder a Environment
decodeArgEnvironment = encoderFor $ \xml ->
   liftM makeEnvironment $ sequence
      [ decodeBinding // x
      | x <- children xml
      , name x == "argument"
      ]
 
decodeBinding :: XMLDecoder a Binding
decodeBinding = encoderFor $ \xml -> do
   a <- findAttribute "description" xml
   isOM <- withState isOpenMath
   case findChild "OMOBJ" xml of
      -- OpenMath object found inside tag
      Just this | isOM -> 
         case xml2omobj this >>= fromOMOBJ of
            Left err   -> fail err
            Right term -> return (termBinding a term)
      -- Simple value
      _ -> return (makeBinding (makeRef a) (getData xml))
 where
   termBinding :: String -> Term -> Binding
   termBinding = makeBinding . makeRef