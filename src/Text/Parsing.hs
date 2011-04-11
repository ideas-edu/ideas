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
   ( module Text.ParserCombinators.Parsec
   , module Text.ParserCombinators.Parsec.Expr
   , module Text.ParserCombinators.Parsec.Language
   , (<*>), (*>), (<*), (<$>), (<$), (<**>)
   , parseSimple, complete, accepts, skip, (<..>), ranges, stopOn
   , signFloat
   ) where
   
import Control.Arrow
import Control.Applicative hiding ((<|>))
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

parseSimple :: Parser a -> String -> Either String a
parseSimple p = left show . runParser (complete p) () ""

complete :: Parser a -> Parser a
complete p = spaces *> (p <* eof)

accepts :: Parser a -> String -> Bool
accepts p = either (const False) (const True) . parseSimple p

skip :: Parser a -> Parser ()
skip p = p >> return ()

infix  6 <..>

(<..>) :: Char -> Char -> Parser Char
x <..> y = satisfy (\c -> c >= x && c <= y)

ranges :: [(Char, Char)] -> Parser Char
ranges xs = choice [ a <..> b | (a, b) <- xs ]

signFloat :: P.TokenParser () -> Parser Double
signFloat l = option id (negate <$ char '-') <*> P.float l

-- return in local function f needed for backwards compatibility
stopOn :: [String] -> Parser String
stopOn ys = rec
 where
   stop = choice (map f ys)
   f x  = try (string x >> return ' ')
   rec  =  (:) <$ notFollowedBy stop <*> anyChar <*> rec
       <|> return []
