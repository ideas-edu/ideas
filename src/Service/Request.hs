-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Service.Request where

import Common.Library hiding (exerciseId)
import Data.Char

data Request = Request
   { service    :: String
   , exerciseId :: Maybe Id
   , source     :: Maybe String
   , dataformat :: DataFormat
   , encoding   :: Maybe Encoding
   }
   
data DataFormat = XML | JSON 

data Encoding = OpenMath | StringEncoding
   
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
      _          -> fail $ "Invalid encoding: " ++ xs