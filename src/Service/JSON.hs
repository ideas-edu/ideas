{-# OPTIONS -XTypeSynonymInstances #-}
module Service.JSON 
   ( JSON(..), Key, Number(..)            -- types
   , ToJSON(..)                           -- type class
   , parseJSON, showCompact, showPretty   -- parser and pretty-printers
   ) where

import Common.Parsing
import Common.Utils (indent)
import Data.List (intersperse)
import qualified UU.Parsing

-- temporary test
main :: IO ()
main = do
   input <- readFile "ex.json"
   print (scan input)
   putStrLn input
   let Just json = parseJSON input
   print json
   print (parseJSON $ show json)
   
data JSON 
   = Number  Number        -- integer, real, or floating point
   | String  String        -- double-quoted Unicode with backslash escapement
   | Boolean Bool          -- true and false
   | Array   [JSON]        -- ordered sequence (comma-separated, square brackets)
   | Object  [(Key, JSON)] -- collection of key/value pairs (comma-separated, curly brackets
   | Null

type Key = String
          
data Number = I Integer | F Float

instance Show JSON where
   show = showPretty
       
showCompact :: JSON -> String
showCompact json =
   case json of
      Number n  -> show n
      String s  -> show s
      Boolean b -> if b then "true" else "false"
      Array xs  -> squareBrackets $ concat $ intersperse ", " $ map showCompact xs
      Object xs -> let f (k, v) = show k ++ ": " ++ showCompact v
                   in curlyBrackets  $ concat $ intersperse ", " $ map f xs
      Null      -> "null"
  
showPretty :: JSON -> String
showPretty json =
   case json of
      Array xs  -> squareBrackets $ "\n" ++ indent 3 (commas (map showPretty xs))
      Object xs -> let f (k, v) = show k ++ ": " ++ showPretty v
                   in curlyBrackets $ "\n" ++ indent 3 (commas (map f xs))
      _         -> showCompact json
 where      
   commas []     = []
   commas [x]    = x
   commas (x:xs) = x ++ ",\n" ++ commas xs
         
instance Show Number where
   show (I n) = show n
   show (F f) = show f

class ToJSON a where
   toJSON :: a -> JSON

instance ToJSON Int     where toJSON = toJSON . toInteger
instance ToJSON Integer where toJSON = Number . I
instance ToJSON Float   where toJSON = Number . F
instance ToJSON String  where toJSON = String
instance ToJSON Bool    where toJSON = Boolean
instance ToJSON a => ToJSON [a] where toJSON = Array . map toJSON

parseJSON :: String -> Maybe JSON
parseJSON input = 
   case parse json (scanWith (makeCharsSpecial ":" defaultScanner) input) of 
      (result, []) -> Just result
      _            -> Nothing
 where
   json :: TokenParser JSON
   json =  (Number . I) <$> pInteger
       <|> (Number . F) <$> pFraction
       <|> String <$> pString
       <|> Boolean True <$ pKey "true"
       <|> Boolean False <$ pKey "false"
       <|> Array <$> pBracks (pCommas json)
       <|> Object <$> pCurly (pCommas keyValue)
       <|> Null <$ pKey "null"

   keyValue :: TokenParser (String, JSON)
   keyValue = (,) <$> pString <* pSpec ':' <*> json

squareBrackets, curlyBrackets :: String -> String
squareBrackets s = "[" ++ s ++ "]"
curlyBrackets  s = "{" ++ s ++ "}"