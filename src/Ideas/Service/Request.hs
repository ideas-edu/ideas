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
-----------------------------------------------------------------------------
--  $Id$

module Ideas.Service.Request where

import Data.Char
import Data.List
import Data.Maybe
import Ideas.Common.Library hiding (exerciseId)
import Ideas.Common.Utils

data Request = Request
   { serviceId      :: Maybe Id
   , exerciseId     :: Maybe Id
   , user           :: Maybe String
   , source         :: Maybe String
   , feedbackScript :: Maybe String
   , cgiBinary      :: Maybe String
   , dataformat     :: DataFormat
   , encoding       :: [Encoding]
   }

emptyRequest :: Request
emptyRequest = Request Nothing Nothing Nothing Nothing Nothing Nothing XML []

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