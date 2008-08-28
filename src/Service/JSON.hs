-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Support for JavaScript Object Notation (JSON) and remote procedure calls using 
-- JSON. JSON is a lightweight alternative for XML. 
--
-----------------------------------------------------------------------------
module Service.JSON 
   ( JSON(..), Key, Number(..)            -- types
   , InJSON(..)                           -- type class
   , parseJSON, showCompact, showPretty   -- parser and pretty-printers
   , jsonRPC, JSON_RPC_Handler
   ) where

import Common.Parsing
import Common.Utils (indent)
import Data.List (intersperse)
import Control.Monad

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

class InJSON a where
   toJSON       :: a -> JSON
   listToJSON   :: [a] -> JSON
   fromJSON     :: JSON -> Maybe a
   listFromJSON :: JSON -> Maybe [a]
   -- default definitions
   listToJSON   = Array . map toJSON
   listFromJSON (Array xs) = mapM fromJSON xs
   listFromJSON _          = Nothing

instance InJSON Int where 
   toJSON   = toJSON . toInteger
   fromJSON = fmap fromInteger . fromJSON
   
instance InJSON Integer where 
   toJSON                  = Number . I
   fromJSON (Number (I n)) = Just n
   fromJSON _              = Nothing

instance InJSON Float where 
   toJSON = Number . F
   fromJSON (Number (F n)) = Just n
   fromJSON _              = Nothing
   
instance InJSON Char where
   toJSON c   = String [c]
   listToJSON = String
   fromJSON (String [c]) = Just c
   fromJSON _ = Nothing
   listFromJSON (String s) = Just s
   listFromJSON _ = Nothing

instance InJSON Bool where 
   toJSON = Boolean
   fromJSON (Boolean b) = Just b
   fromJSON _           = Nothing

instance InJSON a => InJSON [a] where 
   toJSON   = listToJSON
   fromJSON = listFromJSON

instance (InJSON a, InJSON b) => InJSON (a, b) where
   toJSON (a, b)           = Array [toJSON a, toJSON b]
   fromJSON (Array [a, b]) = liftM2 (,) (fromJSON a) (fromJSON b)
   fromJSON _              = Nothing

instance (InJSON a, InJSON b, InJSON c) => InJSON (a, b, c) where
   toJSON (a, b, c)           = Array [toJSON a, toJSON b, toJSON c]
   fromJSON (Array [a, b, c]) = liftM3 (,,) (fromJSON a) (fromJSON b) (fromJSON c)
   fromJSON _                 = Nothing

instance (InJSON a, InJSON b, InJSON c, InJSON d) => InJSON (a, b, c, d) where
   toJSON (a, b, c, d)           = Array [toJSON a, toJSON b, toJSON c, toJSON d]
   fromJSON (Array [a, b, c, d]) = liftM4 (,,,) (fromJSON a) (fromJSON b) (fromJSON c) (fromJSON d)
   fromJSON _                    = Nothing
    
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

--------------------------------------------------------
-- JSON-RPC

data JSON_RPC_Request = Request
   { requestMethod :: String
   , requestParams :: JSON
   , requestId     :: JSON
   }
   
data JSON_RPC_Response = Response
   { responseResult :: JSON
   , responseError  :: JSON
   , responseId     :: JSON
   }

instance Show JSON_RPC_Request where
   show = show . toJSON

instance Show JSON_RPC_Response where
   show = show . toJSON

instance InJSON JSON_RPC_Request where
   toJSON req = Object
      [ ("method", String $ requestMethod req)
      , ("params", requestParams req)
      , ("id"    , requestId req)
      ]
   fromJSON (Object xs) = do
      mj <- lookup "method" xs
      pj <- lookup "params" xs
      ij <- lookup "id"     xs
      case mj of
         String s -> return (Request s pj ij)
         _        -> Nothing
   fromJSON _ = Nothing
         
instance InJSON JSON_RPC_Response where
   toJSON resp = Object
      [ ("result", responseResult resp)
      , ("error" , responseError resp)
      , ("id"    , responseId resp)
      ]
   fromJSON (Object xs) = do
      rj <- lookup "result" xs
      ej <- lookup "error"  xs
      ij <- lookup "id"     xs
      return (Response rj ej ij)
   fromJSON _ = Nothing
   
okResponse :: JSON -> JSON -> JSON_RPC_Response
okResponse x y = Response
   { responseResult = x
   , responseError  = Null
   , responseId     = y
   }
   
errorResponse :: JSON -> JSON -> JSON_RPC_Response
errorResponse x y = Response
   { responseResult = Null
   , responseError  = x
   , responseId     = y
   }
   
--------------------------------------------------------
-- JSON-RPC over HTTP

type JSON_RPC_Handler = String -> JSON -> IO JSON

jsonRPC :: String -> JSON_RPC_Handler -> IO String
jsonRPC input handler = 
         case parseJSON input >>= fromJSON of 
            Nothing   -> fail "Invalid request"
            Just req -> do 
               json <- handler (requestMethod req) (requestParams req)
               return $ show $ okResponse json (requestId req)
             `catch` \e ->
               return $ show $ errorResponse (String (show e)) (requestId req)