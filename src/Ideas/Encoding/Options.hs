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
-----------------------------------------------------------------------------

module Ideas.Encoding.Options
   ( Options, makeOptions, optionBaseUrl
   , script, request, qcGen, baseUrl, maxTime, logRef
   , cgiBin, optionCgiBin, optionHtml
   ) where

import Control.Applicative
import Data.Maybe
import Data.Semigroup as Sem
import Ideas.Encoding.Logging (LogRef)
import Ideas.Encoding.Request
import Ideas.Service.DomainReasoner
import Ideas.Service.FeedbackScript.Parser (parseScriptSafe, Script)
import Test.QuickCheck.Random

-------------------------------------------------------------------
-- Options

cgiBin :: Options -> Maybe String
cgiBin = cgiBinary . request

optionCgiBin :: String -> Options -> Options
optionCgiBin s options = options {request = (request options) {cgiBinary = Just s}}

data Options = Options
   { request   :: Request      -- meta-information about the request
   , qcGen     :: Maybe QCGen  -- random number generator
   , script    :: Script       -- feedback script
   , baseUrl   :: Maybe String -- for html-encoder's css and image files
   , maxTime   :: Maybe Int    -- timeout for services, in seconds
   , logRef    :: LogRef       -- reference for logging to database
   }

instance Sem.Semigroup Options where
   x <> y = Options
      { request   = request x <> request y
      , qcGen     = make qcGen
      , script    = script x <> script y
      , baseUrl   = make baseUrl
      , maxTime   = make maxTime
      , logRef    = logRef x <> logRef y
      }
    where
      make f = f x <|> f y

instance Monoid Options where
   mempty  = Options mempty Nothing mempty Nothing Nothing mempty
   mappend = (<>)

optionHtml :: Options -> Options
optionHtml options = options
   { request = (request options) {encoding = [EncHTML]} }

optionBaseUrl :: String -> Options -> Options
optionBaseUrl base options = options {baseUrl = Just base}

makeOptions :: DomainReasoner -> Request -> IO Options
makeOptions dr req = do
   gen <- maybe newQCGen (return . mkQCGen) (randomSeed req)
   scr <- case feedbackScript req of
             Just s  -> parseScriptSafe s
             Nothing -> defaultScript dr (fromMaybe mempty (exerciseId req))
   return $ mempty
      { request = req
      , qcGen   = Just gen
      , script  = scr
      , maxTime = Just 5
      }