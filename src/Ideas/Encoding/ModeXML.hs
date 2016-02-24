-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
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
import Ideas.Common.Library hiding (exerciseId, (:=))
import Ideas.Common.Utils (Some(..), timedSeconds)
import Ideas.Encoding.DecoderXML
import Ideas.Encoding.Encoder (makeOptions)
import Ideas.Encoding.EncoderHTML
import Ideas.Encoding.EncoderXML
import Ideas.Encoding.Evaluator
import Ideas.Main.Logging (LogRef, changeLog, errormsg)
import Ideas.Service.DomainReasoner
import Ideas.Service.Request
import Ideas.Text.HTML
import Ideas.Text.XML
import System.IO.Error

processXML :: Maybe Int -> Maybe String -> DomainReasoner -> LogRef -> String -> IO (Request, String, String)
processXML maxTime cgiBin dr logRef input = do
   xml  <- either fail return (parseXML input)
   req  <- xmlRequest cgiBin xml
   resp <- maybe id timedSeconds maxTime (xmlReply dr logRef req xml)
    `catch` handler
   let showXML | compactOutput req = compactXML
               | otherwise = show
   if htmlOutput req
      then return (req, showXML resp, "text/html")
      else let out = addVersion (version dr) resp
           in return (req, showXML out, "application/xml")
 where
   handler :: IOException -> IO XML
   handler = resultError logRef . ioeGetErrorString

addVersion :: String -> XML -> XML
addVersion s xml =
   let info = [ "version" := s ]
   in xml { attributes = attributes xml ++ info }

xmlRequest :: Monad m => Maybe String -> XML -> m Request
xmlRequest cgiBin xml = do
   unless (name xml == "request") $
      fail "expected xml tag request"
   enc  <- case findAttribute "encoding" xml of
              Just s  -> readEncoding s
              Nothing -> return []
   return emptyRequest
      { serviceId      = fmap newId $ findAttribute "service" xml
      , exerciseId     = extractExerciseId xml
      , source         = findAttribute "source" xml
      , cgiBinary      = cgiBin
      , requestInfo    = findAttribute "requestinfo" xml
      , logSchema      = findAttribute "logging" xml >>= readSchema
      , feedbackScript = findAttribute "script" xml
      , dataformat     = XML
      , encoding       = enc
      }

xmlReply :: DomainReasoner -> LogRef -> Request -> XML -> IO XML
xmlReply dr logRef request xml = do
   srv <- case serviceId request of
             Just a  -> findService dr a
             Nothing -> fail "No service"

   Some options <- makeOptions dr request

   if htmlOutput request
      -- HTML evaluator
      then liftM toXML $ evalService logRef options (htmlEvaluator dr) srv xml
      -- xml evaluator
      else liftM resultOk $ evalService logRef options xmlEvaluator srv xml

extractExerciseId :: Monad m => XML -> m Id
extractExerciseId = liftM newId . findAttribute "exerciseid"

resultOk :: XMLBuilder -> XML
resultOk body = makeXML "reply" $
   ("result" .=. "ok")
   <> body

resultError :: LogRef -> String -> IO XML
resultError logRef msg = do
   changeLog logRef (\r -> r {errormsg = msg})
   return $ makeXML "reply" $
      ("result" .=. "error")
      <> tag "message" (string msg)

------------------------------------------------------------

xmlEvaluator :: Evaluator a XML XMLBuilder
xmlEvaluator = Evaluator xmlDecoder xmlEncoder

htmlEvaluator :: DomainReasoner -> Evaluator a XML HTMLPage
htmlEvaluator dr = Evaluator xmlDecoder (htmlEncoder dr)
