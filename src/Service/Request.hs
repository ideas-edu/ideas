module Service.Request where

import Common.Exercise
import Data.Char

data Request = Request
   { service    :: String
   , exerciseID :: ExerciseCode
   , source     :: Maybe String
   , dataformat :: DataFormat
   , encoding   :: Maybe Encoding
   }
   deriving Show
   
data DataFormat = XML | JSON 
   deriving Show

data Encoding = OpenMath | StringEncoding
   deriving Show
   
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