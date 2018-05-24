-----------------------------------------------------------------------------
-- Copyright 2018, Ideas project team. This file is distributed under the
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
   , script, request, qcGen, baseUrl, maxTime
   , cgiBin, optionCgiBin, optionHtml
   ) where

import Control.Applicative
import Data.Monoid
import Data.Semigroup as Sem
import Ideas.Common.Library (Exercise, getId)
import Ideas.Encoding.Request
import Ideas.Service.DomainReasoner
import Ideas.Service.FeedbackScript.Parser (parseScriptSafe, Script)
import Test.QuickCheck.Random

-------------------------------------------------------------------
-- Options

cgiBin :: Options -> Maybe String
cgiBin = cgiBinary . request

optionCgiBin :: String -> Options
optionCgiBin s = mempty {request = mempty {cgiBinary = Just s}}

data Options = Options
   { request  :: Request      -- meta-information about the request
   , qcGen    :: Maybe QCGen  -- random number generator
   , script   :: Script       -- feedback script
   , baseUrl  :: Maybe String -- for html-encoder's css and image files
   , maxTime  :: Maybe Int    -- timeout for services, in seconds
   }

instance Sem.Semigroup Options where
   x <> y = Options
      { request  = request x <> request y
      , qcGen    = make qcGen
      , script   = script x <> script y
      , baseUrl  = make baseUrl
      , maxTime  = make maxTime
      }
    where
      make f = f x <|> f y

instance Monoid Options where
   mempty  = Options mempty Nothing mempty Nothing Nothing
   mappend = (<>)

optionHtml :: Options
optionHtml = mempty
   { request = mempty {encoding = [EncHTML]} }

optionBaseUrl :: String -> Options
optionBaseUrl base = mempty {baseUrl = Just base}

makeOptions :: DomainReasoner -> Exercise a -> Request -> IO Options
makeOptions dr ex req = do
   gen <- maybe newQCGen (return . mkQCGen) (randomSeed req)
   scr <- case feedbackScript req of
             Just s  -> parseScriptSafe s
             Nothing -> defaultScript dr (getId ex)
   return $ mempty
      { request  = req
      , qcGen    = Just gen
      , script   = scr
      }