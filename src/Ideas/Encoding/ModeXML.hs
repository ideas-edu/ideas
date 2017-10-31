-----------------------------------------------------------------------------
-- Copyright 2016, Ideas project team. This file is distributed under the
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
import Ideas.Common.Library hiding (exerciseId)
import Ideas.Encoding.DecoderXML
import Ideas.Encoding.EncoderHTML
import Ideas.Encoding.EncoderXML
import Ideas.Encoding.Evaluator
import Ideas.Encoding.Logging (LogRef, changeLog, errormsg)
import Ideas.Encoding.Options (Options, makeOptions, maxTime, cgiBin)
import Ideas.Encoding.Request
import Ideas.Service.DomainReasoner
import Ideas.Text.HTML
import Ideas.Text.XML
import Ideas.Utils.Prelude (timedSeconds)
import System.IO.Error

processXML :: Options -> DomainReasoner -> LogRef -> String -> IO (Request, String, String)
processXML options dr logRef txt = do
   xml  <- either fail return (parseXML txt)
   req  <- xmlRequest (cgiBin options) xml
   resp <- maybe id timedSeconds (maxTime options) (xmlReply options dr logRef req xml)
    `catch` handler
   let showXML | compactOutput req = compactXML
               | otherwise = show
       showHtmlDoc doc = "<!DOCTYPE html>" ++ compactXML doc
   if htmlOutput req
      then return (req, showHtmlDoc resp, "text/html")
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
xmlRequest ms xml = do
   unless (name xml == "request") $
      fail "expected xml tag request"
   enc  <- case findAttribute "encoding" xml of
              Just s  -> readEncoding s
              Nothing -> return []
   return mempty
      { serviceId      = newId <$> findAttribute "service" xml
      , exerciseId     = extractExerciseId xml
      , source         = findAttribute "source" xml
      , cgiBinary      = ms
      , requestInfo    = findAttribute "requestinfo" xml
      , logSchema      = findAttribute "logging" xml >>= readSchema
      , feedbackScript = findAttribute "script" xml
      , randomSeed     = defaultSeed ms $
                            findAttribute "randomseed" xml >>= readM
      , dataformat     = Just XML
      , encoding       = enc
      }

-- Use a fixed seed for random number generation for command-line invocations
defaultSeed :: Maybe String -> Maybe Int -> Maybe Int
defaultSeed Nothing Nothing = Just 2805 -- magic number
defaultSeed _ m = m

xmlReply :: Options -> DomainReasoner -> LogRef -> Request -> XML -> IO XML
xmlReply opt1 dr logRef request xml = do
   srv <- case serviceId request of
             Just a  -> findService dr a
             Nothing -> fail "No service"

   Some ex <- case exerciseId request of
                 Just a  -> findExercise dr a
                 Nothing -> return (Some emptyExercise)

   opt2 <- makeOptions dr ex request
   let options = opt1 <> opt2

   if htmlOutput request
      -- HTML evaluator
      then toXML <$> evalService logRef ex options (htmlEvaluator dr) srv xml
      -- xml evaluator
      else resultOk <$> evalService logRef ex options xmlEvaluator srv xml

extractExerciseId :: Monad m => XML -> m Id
extractExerciseId = fmap newId . findAttribute "exerciseid"

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