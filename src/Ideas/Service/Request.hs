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
-----------------------------------------------------------------------------

module Ideas.Service.Request where

import Data.Char
import Data.List
import Data.Maybe
import Ideas.Common.Library hiding (exerciseId)
import Ideas.Common.Utils

data Request = Request
   { serviceId      :: Maybe Id
   , exerciseId     :: Maybe Id
   , source         :: Maybe String
   , feedbackScript :: Maybe String
   , requestInfo    :: Maybe String
   , cgiBinary      :: Maybe String
   , logSchema      :: Maybe Schema
   , dataformat     :: DataFormat
   , encoding       :: [Encoding]
   }

emptyRequest :: Request
emptyRequest = Request Nothing Nothing Nothing Nothing
                       Nothing Nothing Nothing XML []

data Schema = V1 | V2 | NoLogging deriving (Show, Eq)

getSchema :: Request -> Schema
getSchema = fromMaybe V1 . logSchema -- log schema V1 is the default

readSchema :: Monad m => String -> m Schema
readSchema s0
   | s == "v1" = return V1
   | s == "v2" = return V2
   | s `elem` ["false", "no"] = return NoLogging
   | otherwise = fail "Unknown schema"
 where
   s = map toLower (filter isAlphaNum s0)

data DataFormat = XML | JSON
   deriving Show -- needed for LoggingDatabase

data Encoding = EncHTML      -- html page as output
              | EncOpenMath  -- encode terms in OpenMath
              | EncString    -- encode terms as strings
              | EncCompact   -- compact ouput
              | EncPretty    -- pretty output
 deriving Eq

instance Show Encoding where
   showList xs rest = intercalate "+" (map show xs) ++ rest
   show EncHTML     = "html"
   show EncOpenMath = "openmath"
   show EncString   = "string"
   show EncCompact  = "compact"
   show EncPretty   = "pretty"

htmlOutput :: Request -> Bool
htmlOutput = (EncHTML `elem`) . encoding

compactOutput :: Request -> Bool
compactOutput req =
   case (EncCompact `elem` xs, EncPretty `elem` xs) of
      (True, False) -> True
      (False, True) -> False
      _             -> isJust (cgiBinary req)
 where
   xs = encoding req

useOpenMath :: Request -> Bool
useOpenMath r = all (`notElem` encoding r) [EncString, EncHTML]

useLogging :: Request -> Bool
useLogging = (EncHTML `notElem`) . encoding

discoverDataFormat :: Monad m => String -> m DataFormat
discoverDataFormat xs =
   case dropWhile isSpace xs of
      '<':_ -> return XML
      '{':_ -> return JSON
      _     -> fail "Unknown data format"

readEncoding :: Monad m => String -> m [Encoding]
readEncoding = mapM (f . map toLower) . splitsWithElem '+'
 where
   f "html"     = return EncHTML
   f "openmath" = return EncOpenMath
   f "string"   = return EncString
   f "compact"  = return EncCompact
   f "pretty"   = return EncPretty
   f s          = fail $ "Invalid encoding: " ++ s