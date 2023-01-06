-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Ideas.Text.JSON.Data
   ( JSON(..), Key            -- types
   , lookupM
   , parseJSON, compactJSON   -- parser and pretty-printers
   ) where

import Control.Monad
import Data.List (intersperse)
import Data.Maybe
import Data.String
import Ideas.Utils.Parsing hiding (string, char)
import Test.QuickCheck
import Text.PrettyPrint.Leijen hiding ((<$>))
import qualified Ideas.Text.UTF8 as UTF8
import qualified Text.ParserCombinators.Parsec.Token as P

data JSON
   = Integer Integer       -- integer, ... 
   | Double  Double        --   ... real, or floating point
   | String  String        -- double-quoted Unicode with backslash escapement
   | Boolean Bool          -- true and false
   | Array   [JSON]        -- ordered sequence (comma-separated, square brackets)
   | Object  [(Key, JSON)] -- collection of key/value pairs (comma-separated, curly brackets
   | Null
 deriving Eq

type Key = String

instance Show JSON where
   show = show . prettyJSON False

instance IsString JSON where
   fromString = String

compactJSON :: JSON -> String
compactJSON = show . prettyJSON True

prettyJSON :: Bool -> JSON -> Doc
prettyJSON compact = rec
 where
   rec json =
      case json of
         Integer n -> text (show n)
         Double d  -> text (show d)
         String s  -> str (escape s)
         Boolean b -> text (if b then "true" else "false")
         Null      -> text "null"
         Array xs  -> make lbracket rbracket (map rec xs)
         Object xs -> make lbrace rbrace (map (uncurry (<:>)) xs)

   x <:> y | compact    = str x <> char ':' <> rec y
           | isSimple y = str x <> string ": " <> rec y
           | otherwise  = align (str x <> char ':' <> line <> indent 2 (rec y))

   str = dquotes . text

   make open close xs
      | compact || length xs < 2 =
           enclose open close (hcat (intersperse comma xs))
      | otherwise =
           align (vsep (zipWith (<+>) (open:repeat comma) xs ++ [close]))

   isSimple (Array xs)  = null xs
   isSimple (Object xs) = null xs
   isSimple _           = True

-- Escape double quote and backslash, and convert to UTF8 encoding
escape :: String -> String
escape = concatMap f . fromMaybe "invalid UTF8 string" . UTF8.encode
 where
   f '\n' = "\\n"
   f '\r' = ""      -- carriage return (DOS files)
   f '\t' = "\\t"
   f '"'  = "\\\""
   f '\\' = "\\\\"
   f c    = [c]

--------------------------------------------------------
-- Parser

parseJSON :: String -> Either String JSON
parseJSON = parseSimple json
 where
   json :: Parser JSON
   json = choice
      [ Null          <$ P.reserved lexer "null"
      , Boolean True  <$ P.reserved lexer "true"
      , Boolean False <$ P.reserved lexer "false"
      , either Integer Double <$> naturalOrFloat -- redefined in Ideas.Text.Parsing
      , String <$> P.stringLiteral lexer
      , Array  <$> P.brackets lexer (sepBy json (P.comma lexer))
      , Object <$> P.braces lexer (sepBy keyValue (P.comma lexer))
      ]

   keyValue :: Parser (String, JSON)
   keyValue = (,) <$> P.stringLiteral lexer <* P.colon lexer <*> json

   lexer :: P.TokenParser a
   lexer = P.makeTokenParser $ emptyDef
      { reservedNames = ["true", "false", "null"] }

lookupM :: String -> JSON -> Either String JSON
lookupM x (Object xs) = maybe (Left $ "field " ++ x ++ " not found") Right (lookup x xs)
lookupM _ _ = Left "expecting a JSON object"

--------------------------------------------------------
-- Testing parser/pretty-printer

instance Arbitrary JSON where
   arbitrary = sized arbJSON

arbJSON :: Int -> Gen JSON
arbJSON n
   | n == 0 = oneof
        [ Integer <$> arbitrary, Double . fromInteger <$> arbitrary 
        , String <$> myStringGen
        , Boolean <$> arbitrary, return Null
        ]
   | otherwise = oneof
        [ arbJSON 0
        , do i  <- choose (0, 6)
             xs <- replicateM i rec
             return (Array xs)
        , do i  <- choose (0, 6)
             xs <- replicateM i myStringGen
             ys <- replicateM i rec
             return (Object (zip xs ys))
        ]
 where
   rec = arbJSON (n `div` 2)

myStringGen :: Gen String
myStringGen = do
   n <- choose (1, 10)
   replicateM n $ elements $
      ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9']

_propEncoding :: Property
_propEncoding = property $ \a ->
   parseJSON (show a) == Right a