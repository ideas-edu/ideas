{-# OPTIONS -XTypeSynonymInstances #-}
module Service.JSON 
   ( JSON(..), Key, Number(..)            -- types
   , ToJSON(..)                           -- type class
   , parseJSON, showCompact, showPretty   -- parser and pretty-printers
   ) where

import Common.Parsing
import Common.Utils (indent)
import Data.List (intersperse)
import Network.CGI hiding (requestMethod)
import Control.Monad.Trans
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

--------------------------------------------------------
-- JSON-RPC

data JSON_RPC_Request = Request
   { requestMethod :: String
   , requestParams :: [JSON]
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

instance ToJSON JSON_RPC_Request where
   toJSON req = Object
      [ ("method", String $ requestMethod req)
      , ("params", Array  $ requestParams req)
      , ("id"    , requestId req)
      ]

instance ToJSON JSON_RPC_Response where
   toJSON resp = Object
      [ ("result", responseResult resp)
      , ("error" , responseError resp)
      , ("id"    , responseId resp)
      ]

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

type JSON_RPC_Handler = String -> [JSON] -> IO (JSON, Bool)

jsonOverHTTP :: JSON_RPC_Handler -> IO ()
jsonOverHTTP handler = runCGI $ do
   -- get input
   raw  <- getInput "input"     -- read input
   addr <- remoteAddr           -- the IP address of the remote host making the request
   
   case raw of
      Nothing -> fail "No request"
      Just input -> 
         case parseRequest input of 
            Left err  -> fail err
            Right req -> do 
               (json, b) <- lift $ handler (requestMethod req) (requestParams req)
               let f = if b then okResponse else errorResponse
               setHeader "Content-type" "application/json"
               output $ show $ f json (requestId req)
               
 
 where
   lookupM :: (Show a, Eq a, Monad m) => a -> [(a, b)] -> m b
   lookupM a xs = maybe (fail $ "Element " ++ show a ++ " not found") return (lookup a xs)
 
   parseRequest :: String -> Either String JSON_RPC_Request
   parseRequest input =
      case parseJSON input of
         Just (Object xs) -> do
            m  <- lookupM "method" xs
            ms <- case m of
                     String s -> return s
                     _        -> fail "method is not a string"
            s  <- lookupM "params" xs
            ps <- case s of
                     Array xs -> return xs
                     _        -> return [s] 
            i  <- lookupM "id"     xs
            return $ Request  
               { requestMethod = ms
               , requestParams = ps
               , requestId     = i
               }
         Just _    -> fail "not a JSON object"
         Nothing   -> fail "parse error"