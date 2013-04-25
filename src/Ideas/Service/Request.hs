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
-----------------------------------------------------------------------------
module Ideas.Service.Request where

import Ideas.Common.Library hiding (exerciseId)
import Data.Char

data Request = Request
   { service    :: String
   , exerciseId :: Maybe Id
   , source     :: Maybe String
   , dataformat :: DataFormat
   , encoding   :: Maybe Encoding
   }

data DataFormat = XML | JSON
   deriving Show -- needed for LoggingDatabase

data Encoding = OpenMath | StringEncoding | HTMLEncoding
   deriving (Show, Eq) -- show needed for LoggingDatabase

discoverDataFormat :: Monad m => String -> m DataFormat
discoverDataFormat xs =
   case dropWhile isSpace xs of
      '<':_ -> return XML
      '{':_ -> return JSON
      _     -> fail "Unknown data format"

readEncoding :: Monad m => String -> m Encoding
readEncoding xs =
   case map toLower xs of
      "openmath" -> return OpenMath
      "string"   -> return StringEncoding
      "html"     -> return HTMLEncoding
      _          -> fail $ "Invalid encoding: " ++ xs