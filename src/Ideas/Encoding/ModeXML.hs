-----------------------------------------------------------------------------
-- Copyright 2013, Open Universiteit Nederland. This file is distributed
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
module Ideas.Encoding.ModeXML (processXML) where

import Control.Exception
import Control.Monad
import Control.Monad.Error
import Data.Maybe
import Ideas.Common.Library hiding (exerciseId, (:=))
import Ideas.Common.Utils (Some(..), timedSeconds)
import Ideas.Encoding.DecoderXML
import Ideas.Encoding.EncoderHTML
import Ideas.Encoding.EncoderXML
import Ideas.Encoding.Evaluator
import Ideas.Encoding.LinkManager
import Ideas.Encoding.OpenMathSupport
import Ideas.Service.DomainReasoner
import Ideas.Service.FeedbackScript.Parser (parseScriptSafe)
import Ideas.Service.FeedbackScript.Syntax (Script)
import Ideas.Service.Request
import Ideas.Text.HTML
import Ideas.Text.OpenMath.Object
import Ideas.Text.XML
import System.IO.Error
import System.Random (StdGen, newStdGen)

processXML :: Maybe Int -> DomainReasoner -> Maybe String -> String -> IO (Request, String, String)
processXML maxTime dr cgiBin input = do
   xml  <- either fail return (parseXML input)
   req  <- either fail return (xmlRequest xml)
   resp <- maybe id timedSeconds maxTime (xmlReply dr cgiBin req xml)
    -- to do: improve catching errors (old and new styles are mixed)
    `catchError` (return . resultError . ioeGetErrorString)
    `catch` (\(SomeException e) -> return (resultError (show e)))
   let showXML | compactOutputDefault (isJust cgiBin) req = compactXML 
               | otherwise = show
   if htmlOutput req 
      then return (req, showXML resp, "text/html")
      else let out = addVersion (version dr) resp
           in return (req, showXML out, "application/xml")

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
              Just s  -> readEncoding s
              Nothing -> return []
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
   
   -- HTML encoder 
   if htmlOutput request
      then do
         res <- evalService (htmlConverter dr cgiBin script ex stdgen xml) srv
         return (toXML res) 
      -- OpenMath encoder
      else if useOpenMath request 
      then do
         res <- evalService (openMathConverter True script ex stdgen xml) srv
         return (resultOk res)
      -- String encoder
      else do
         res <- evalService (stringFormatConverter script ex stdgen xml) srv
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

stringFormatConverter :: Script -> Exercise a -> StdGen -> XML -> Evaluator a XMLBuilder
stringFormatConverter script ex stdgen xml =
   Evaluator (runEncoderStateM xmlEncoder xes)
             (\tp -> runEncoderStateM (xmlDecoder tp) xds xml)
 where
   xes = XMLEncoderState ex False (tag "expr" . string . prettyPrinter ex)
   xds = XMLDecoderState ex script stdgen False g
   g = (liftM getData . findChild "expr") >=> parser ex

htmlConverter :: DomainReasoner -> Maybe String -> Script -> Exercise a -> StdGen -> XML -> Evaluator a HTMLPage
htmlConverter dr cgiBin script ex stdgen xml =
   Evaluator (return . htmlEncoder lm dr ex) d
 where
   lm = maybe staticLinks dynamicLinks cgiBin
   Evaluator _ d = stringFormatConverter script ex stdgen xml

openMathConverter :: Bool -> Script -> Exercise a -> StdGen -> XML -> Evaluator a XMLBuilder
openMathConverter withMF script ex stdgen xml =
   Evaluator (runEncoderStateM xmlEncoder xes)
             (\tp -> runEncoderStateM (xmlDecoder tp) xds xml)
 where
   xes = XMLEncoderState ex True h
   xds = XMLDecoderState ex script stdgen True g
   h a = case toOpenMath ex a of
            Left _      -> error "Error encoding term in OpenMath" -- fix me!
            Right omobj -> builder (toXML (handleMixedFractions omobj))
   g xml0 = do
      xob <- findChild "OMOBJ" xml0
      case xml2omobj xob of
         Left  msg   -> Left msg
         Right omobj ->
            case fromOpenMath ex omobj of
              Just a  -> Right a
              Nothing -> Left "Invalid OpenMath object for this exercise"
   -- Remove special mixed-fraction symbol (depending on boolean argument)
   handleMixedFractions = if withMF then id else noMixedFractions