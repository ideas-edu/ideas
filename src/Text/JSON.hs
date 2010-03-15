-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
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
module Text.JSON 
   ( JSON(..), Key, Number(..)            -- types
   , InJSON(..)                           -- type class"
   , lookupM
   , parseJSON, showCompact, showPretty   -- parser and pretty-printers
   , jsonRPC, JSON_RPC_Handler, testMe
   ) where

import Text.Parsing
import qualified Text.UTF8 as UTF8
import Common.Utils (indent)
import Data.Char
import Data.List (intersperse)
import Data.Maybe
import Control.Monad
import Service.Revision (version, revision)
import Test.QuickCheck

data JSON 
   = Number  Number        -- integer, real, or floating point
   | String  String        -- double-quoted Unicode with backslash escapement
   | Boolean Bool          -- true and false
   | Array   [JSON]        -- ordered sequence (comma-separated, square brackets)
   | Object  [(Key, JSON)] -- collection of key/value pairs (comma-separated, curly brackets
   | Null
 deriving Eq

type Key = String
          
data Number = I Integer | F Float deriving Eq

instance Show JSON where
   show = showPretty
       
showCompact :: JSON -> String
showCompact json =
   case json of
      Number n  -> show n
      String s  -> "\"" ++ escape s ++ "\""
      Boolean b -> if b then "true" else "false"
      Array xs  -> squareBrackets $ concat $ intersperse ", " $ map showCompact xs
      Object xs -> let f (k, v) = show k ++ ": " ++ showCompact v
                   in curlyBrackets  $ concat $ intersperse ", " $ map f xs
      Null      -> "null"
  
-- Escape double quote and backslash, and convert to UTF8 encoding
escape :: String -> String
escape = concatMap f . fromMaybe "invalid UTF8 string" . UTF8.encodeM 
 where
   f '"'  = "\\\""
   f '\\' = "\\\\"
   f c    = [c]
  
showPretty :: JSON -> String
showPretty json =
   case json of
      Array xs  -> squareBrackets $ '\n' : indent 3 (commas (map showPretty xs))
      Object xs -> let f (k, v) = show k ++ ": " ++ showPretty v
                   in curlyBrackets $ '\n' : indent 3 (commas (map f xs))
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
   fromJSON     :: Monad m => JSON -> m a
   listFromJSON :: Monad m => JSON -> m [a]
   -- default definitions
   listToJSON   = Array . map toJSON
   listFromJSON (Array xs) = mapM fromJSON xs
   listFromJSON _          = fail "expecting an array"

instance InJSON Int where 
   toJSON   = toJSON . toInteger
   fromJSON = liftM fromInteger . fromJSON
   
instance InJSON Integer where 
   toJSON                  = Number . I
   fromJSON (Number (I n)) = return n
   fromJSON _              = fail "expecting a number"

instance InJSON Float where 
   toJSON = Number . F
   fromJSON (Number (F n)) = return n
   fromJSON _              = fail "expecting a number"
   
instance InJSON Char where
   toJSON c   = String [c]
   listToJSON = String
   fromJSON (String [c]) = return c
   fromJSON _ = fail "expecting a string"
   listFromJSON (String s) = return s
   listFromJSON _ = fail "expecting a string"

instance InJSON Bool where 
   toJSON = Boolean
   fromJSON (Boolean b) = return b
   fromJSON _           = fail "expecting a boolean"

instance InJSON a => InJSON [a] where 
   toJSON   = listToJSON
   fromJSON = listFromJSON

instance (InJSON a, InJSON b) => InJSON (a, b) where
   toJSON (a, b)           = Array [toJSON a, toJSON b]
   fromJSON (Array [a, b]) = liftM2 (,) (fromJSON a) (fromJSON b)
   fromJSON _              = fail "expecting an array with 2 elements"

instance (InJSON a, InJSON b, InJSON c) => InJSON (a, b, c) where
   toJSON (a, b, c)           = Array [toJSON a, toJSON b, toJSON c]
   fromJSON (Array [a, b, c]) = liftM3 (,,) (fromJSON a) (fromJSON b) (fromJSON c)
   fromJSON _                 = fail "expecting an array with 3 elements"

instance (InJSON a, InJSON b, InJSON c, InJSON d) => InJSON (a, b, c, d) where
   toJSON (a, b, c, d)           = Array [toJSON a, toJSON b, toJSON c, toJSON d]
   fromJSON (Array [a, b, c, d]) = liftM4 (,,,) (fromJSON a) (fromJSON b) (fromJSON c) (fromJSON d)
   fromJSON _                    = fail "expecting an array with 4 elements"
    
parseJSON :: String -> Maybe JSON
parseJSON input = 
   case parse json (scanWith jsonScanner input) of 
      (result, []) -> Just result
      _            -> Nothing
 where
   jsonScanner = makeCharsSpecial ":" defaultScanner
      { keywords   = ["true", "false", "null"]
      , unaryMinus = True
      }
 
   json :: TokenParser JSON
   json =  (Number . I) <$> pInteger
       <|> (Number . F) <$> pFloat
       <|> (String . fromMaybe [] . UTF8.decodeM) <$> pString
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
   fromJSON obj = do
      mj <- lookupM "method" obj
      pj <- lookupM "params" obj
      ij <- lookupM "id"     obj
      case mj of
         String s -> return (Request s pj ij)
         _        -> fail "expecting a string"
         
instance InJSON JSON_RPC_Response where
   toJSON resp = Object
      [ ("result", responseResult resp)
      , ("error" , responseError resp)
      , ("id"    , responseId resp)
      , ("version", String $ version ++ " (" ++ show revision ++ ")")
      ]
   fromJSON obj = do
      rj <- lookupM "result" obj
      ej <- lookupM "error"  obj
      ij <- lookupM "id"     obj
      return (Response rj ej ij)
   
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

lookupM :: Monad m => String -> JSON -> m JSON
lookupM x (Object xs) = maybe (fail $ "field " ++ x ++ " not found") return (lookup x xs)
lookupM _ _ = fail "expecting a JSON object"

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

--------------------------------------------------------
-- Testing parser/pretty-printer

instance Arbitrary JSON where
   arbitrary = sized arbJSON

   coarbitrary json = 
      case json of
         Number a  -> variant 0 . coarbitrary a
         String s  -> variant 1 . coarbitrary s
         Boolean b -> variant 2 . coarbitrary b
         Array xs  -> variant 3 . coarbitrary xs
         Object xs -> variant 4 . coarbitrary xs
         Null      -> variant 5

instance Arbitrary Number where
   arbitrary = oneof [liftM I arbitrary, liftM (F . fromInteger) arbitrary]

   coarbitrary (I n) = variant 0 . coarbitrary n
   coarbitrary (F f) = variant 1 . coarbitrary f

arbJSON :: Int -> Gen JSON
arbJSON n 
   | n == 0 = oneof 
        [ liftM Number arbitrary, liftM String arbitrary
        , liftM Boolean arbitrary, return Null
        ]
   | otherwise = oneof
        [ arbJSON 0
        , do i  <- choose (0, 6)
             xs <- replicateM i rec
             return (Array xs)
        , do i  <- choose (0, 6)
             xs <- replicateM i arbitrary
             ys <- replicateM i rec
             return (Object (zip xs ys))
        ]
 where
   rec = arbJSON (n `div` 2)
   
testMe :: IO ()
testMe = do 
   putStrLn "** JSON encoding"
   quickCheck prop
 where
   prop :: JSON -> Bool
   prop a = parseJSON (show a) == Just a