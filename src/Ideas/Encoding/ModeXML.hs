-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
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
--  $Id$

module Ideas.Encoding.ModeXML (processXML) where

import Control.Exception
import Control.Monad
import Data.Maybe
import Ideas.Common.Library hiding (exerciseId, (:=))
import Ideas.Common.Utils (Some(..), timedSeconds)
import Ideas.Encoding.DecoderXML
import Ideas.Encoding.Encoder (makeOptions)
import Ideas.Encoding.EncoderHTML
import Ideas.Encoding.EncoderXML
import Ideas.Encoding.Evaluator
import Ideas.Service.DomainReasoner
import Ideas.Service.Request
import Ideas.Text.HTML
import Ideas.Text.XML
import Prelude hiding (catch)
import System.IO.Error hiding (catch)

processXML :: Maybe Int -> Maybe String -> DomainReasoner -> String -> IO (Request, String, String)
processXML maxTime cgiBin dr input = do
   xml  <- either fail return (parseXML input)
   req  <- xmlRequest cgiBin xml
   resp <- maybe id timedSeconds maxTime (xmlReply dr req xml)
    `catch` handler
   let showXML | compactOutput req = compactXML
               | otherwise = show
   if htmlOutput req
      then return (req, showXML resp, "text/html")
      else let out = addVersion (version dr) resp
           in return (req, showXML out, "application/xml")
 where
   handler :: IOException -> IO XML
   handler = return . resultError . ioeGetErrorString

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
      , user           = findAttribute "userid" xml
      , source         = findAttribute "source" xml
      , feedbackScript = findAttribute "script" xml
      , cgiBinary      = cgiBin
      , dataformat     = XML
      , encoding       = enc
      }

xmlReply :: DomainReasoner -> Request -> XML -> IO XML
xmlReply dr request xml = do
   srv <- case serviceId request of
             Just a  -> findService dr a
             Nothing -> fail "No service"

   Some options <- makeOptions dr request

   -- HTML evaluator
   if htmlOutput request
      then do
         res <- evalService options (htmlEvaluator dr) srv xml
         return (toXML res)
      -- xml evaluator
      else do
         res <- evalService options xmlEvaluator srv xml
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

xmlEvaluator :: Evaluator a XML XMLBuilder
xmlEvaluator = Evaluator xmlDecoder xmlEncoder

htmlEvaluator :: DomainReasoner -> Evaluator a XML HTMLPage
htmlEvaluator dr = Evaluator xmlDecoder (htmlEncoder dr)