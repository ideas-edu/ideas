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
-- Utility functions for parsing with Parsec library
--
-----------------------------------------------------------------------------
module Text.Parsing
   ( module Export
   , (<*>), (*>), (<*), (<$>), (<$), (<**>)
   , parseSimple, complete, accepts, skip, (<..>), ranges, stopOn
   , naturalOrFloat, float
   ) where
   
import Control.Arrow
import Control.Applicative hiding ((<|>))
import Data.Char
import Data.List
import Text.ParserCombinators.Parsec as Export
import Text.ParserCombinators.Parsec.Expr as Export
import Text.ParserCombinators.Parsec.Language as Export
 
parseSimple :: Parser a -> String -> Either String a
parseSimple p = left show . runParser (complete p) () ""

complete :: Parser a -> Parser a
complete p = spaces *> (p <* eof)

accepts :: Parser a -> String -> Bool
accepts p = either (const False) (const True) . parseSimple p

skip :: Parser a -> Parser ()
skip p = p >> return ()

-- Like the combinator from parser, except that for doubles 
-- the read instance is used. This is a more precies representation
-- of the double (e.g., 1.413 is not 1.413000000001).
naturalOrFloat :: Parser (Either Integer Double) 
naturalOrFloat = do
   a <- num
   b <- option "" ((:) <$> char '.' <*> nat) 
   c <- option "" ((:) <$> oneOf "eE" <*> num)
   spaces 
   case reads (a++b++c) of
      _ | null b && null c -> 
         case a of
            '-':xs -> return (Left (negate (readInt xs)))
            xs     -> return (Left (readInt xs))
      [(d, [])] -> return (Right d)
      _         -> fail "not a float"
 where
   nat = many1 digit
   num = maybe id (:) <$> optionMaybe (char '-') <*> nat
   readInt = foldl' op 0 -- '
   op a b  = a*10+fromIntegral (ord b)-48

float :: Parser Double
float = do
   a <- nat
   b <- option "" ((:) <$> char '.' <*> nat) 
   c <- option "" ((:) <$> oneOf "eE" <*> num)
   case reads (a++b++c) of
      [(d, [])] -> return d
      _         -> fail "not a float"
 where
   nat = many1 digit
   num = (:) <$> char '-' <*> nat

infix  6 <..>

(<..>) :: Char -> Char -> Parser Char
x <..> y = satisfy (\c -> c >= x && c <= y)

ranges :: [(Char, Char)] -> Parser Char
ranges xs = choice [ a <..> b | (a, b) <- xs ]

-- return in local function f needed for backwards compatibility
stopOn :: [String] -> Parser String
stopOn ys = rec
 where
   stop = choice (map f ys)
   f x  = try (string x >> return ' ')
   rec  =  (:) <$ notFollowedBy stop <*> anyChar <*> rec
       <|> return []
