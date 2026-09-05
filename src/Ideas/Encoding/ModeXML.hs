{-# LANGUAGE OverloadedStrings #-}
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
-- Services using XML notation
--
-----------------------------------------------------------------------------

module Ideas.Encoding.ModeXML (processXML) where

import Control.Exception
import Control.Monad
import Data.String
import Ideas.Common.Library hiding (exerciseId)
import Ideas.Encoding.DecoderXML
import Ideas.Encoding.EncoderHTML
import Ideas.Encoding.EncoderXML
import Ideas.Encoding.Evaluator
import Ideas.Encoding.Logging (changeLog, errormsg)
import Ideas.Encoding.Options (Options, makeOptions, maxTime, cgiBin, logRef)
import Ideas.Encoding.Request
import Ideas.Service.DomainReasoner
import Ideas.Text.HTML
import Ideas.Text.XML
import Ideas.Utils.Prelude (timedSeconds)
import System.IO.Error

processXML :: Options -> DomainReasoner -> String -> IO (Request, String, String)
processXML options dr txt = do
   xml  <- either fail return (parseXML txt)
   req  <- xmlRequest (cgiBin options) xml
   resp <- maybe id timedSeconds (maxTime options) (xmlReply options dr req xml)
    `catch` handler
   let showXML | compactOutput req = compactXML
               | otherwise = prettyXML
       showHtmlDoc doc = "<!DOCTYPE html>" ++ compactXML doc
   if htmlOutput req
      then return (req, showHtmlDoc resp, "text/html")
      else let out = addVersion (version dr) resp
           in return (req, showXML out, "application/xml")
 where
   handler :: SomeException -> IO XML
   handler e = resultError options $
      case fromException e of
         Just ioe -> ioeGetErrorString ioe
         Nothing  -> show e

addVersion :: String -> XML -> XML
addVersion s = changeAttributes (<> attribute "version" s)

xmlRequest :: Maybe String -> XML -> IO Request
xmlRequest ms xml = do
   unless (getName xml == "request") $
      fail "expected xml tag request"
   enc  <- case findAttribute' "encoding" xml of
              Just s  -> readEncoding s
              Nothing -> return []
   return mempty
      { serviceId      = newId <$> findAttribute' "service" xml
      , exerciseId     = extractExerciseId xml
      , source         = findAttribute' "source" xml
      , cgiBinary      = ms
      , requestInfo    = findAttribute' "requestinfo" xml
      , logSchema      = findAttribute' "logging" xml >>= readSchema
      , feedbackScript = findAttribute' "script" xml
      , randomSeed     = defaultSeed ms $
                            findAttribute' "randomseed" xml >>= readM
      , dataformat     = Just XML
      , encoding       = enc
      }

-- Use a fixed seed for random number generation for command-line invocations
defaultSeed :: Maybe String -> Maybe Int -> Maybe Int
defaultSeed Nothing Nothing = Just 2805 -- magic number
defaultSeed _ m = m

xmlReply :: Options -> DomainReasoner -> Request -> XML -> IO XML
xmlReply opt1 dr request xml = do
   srv <- case serviceId request of
             Just a  -> either fail return $ findService dr a
             Nothing -> fail "No service"

   Some ex <- case exerciseId request of
                 Just a  -> either fail return $ findExercise dr a
                 Nothing -> return (Some emptyExercise)

   opt2 <- makeOptions dr request
   let options = opt1 <> opt2

   if htmlOutput request
      -- HTML evaluator
      then toXML <$> evalService ex options (htmlEvaluator dr) srv xml
      -- xml evaluator
      else resultOk <$> evalService ex options xmlEvaluator srv xml

extractExerciseId :: XML -> Maybe Id
extractExerciseId = either (const Nothing) (return . newId) . findAttribute "exerciseid"

resultOk :: XMLBuilder -> XML
resultOk body = makeXML (fromString "reply") $
   ("result" .=. "ok")
   <> body

resultError :: Options -> String -> IO XML
resultError options msg = do
   changeLog (logRef options) (\r -> r {errormsg = msg})
   return $ makeXML (fromString "reply") $
      ("result" .=. "error")
      <> tag "message" (string msg)

findAttribute' :: String -> XML -> Maybe String
findAttribute' a = either (const Nothing) Just . findAttribute a

------------------------------------------------------------

xmlEvaluator :: Evaluator a XML XMLBuilder
xmlEvaluator = Evaluator xmlTypeDecoder xmlEncoder

htmlEvaluator :: DomainReasoner -> Evaluator a XML HTMLPage
htmlEvaluator dr = Evaluator xmlTypeDecoder (htmlEncoder dr)