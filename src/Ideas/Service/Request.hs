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
module Ideas.Service.Request where

import Data.Char
import Data.List
import Ideas.Common.Library hiding (exerciseId)
import Ideas.Common.Utils

data Request = Request
   { service    :: String
   , exerciseId :: Maybe Id
   , source     :: Maybe String
   , dataformat :: DataFormat
   , encoding   :: [Encoding]
   }

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
compactOutput = compactOutputDefault True

compactOutputDefault :: Bool -> Request -> Bool
compactOutputDefault b req =
   let xs = encoding req
   in case (EncCompact `elem` xs, EncPretty `elem` xs) of
        (True, False) -> True
        (False, True) -> False
        _             -> b

useOpenMath :: Request -> Bool
useOpenMath = (EncString `notElem`) . encoding

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