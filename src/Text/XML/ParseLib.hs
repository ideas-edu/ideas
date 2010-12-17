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
-- An interface to a parsing library. The Parsec library is used because
-- we need the back-tracking facilities (try combinator) in order not to
-- have to rewrite the complete grammar for XML. In addition, the stopOn
-- combinator is hard (or impossible?) to write using the UU library. This
-- abstraction should make it easier to switch to a different parsing library
-- in future, in case we want to.
--
-----------------------------------------------------------------------------
module Text.XML.ParseLib 
   ( Parser, (<|>), (<..>)
   , symbol, string, optionM, option, oneOf, ranges, many, doubleQuoted, bracketed
   , singleQuoted, stopOn, parenthesized, choice, many1, try
   , noneOf, chainr1, sepBy1, parse, accept, packed, recognize
   ) where

import qualified Text.ParserCombinators.Parsec as P

infix  6 <..>
infixr 4 <|>

type Parser a = P.CharParser () a

parse :: Parser a -> String -> Either String a
parse p = either (Left . show) Right . P.parse (p >>= \a -> P.eof >> return a) ""

accept :: Parser a -> String -> Bool
accept p = either (const False) (const True) . parse p
      
try :: Parser a -> Parser a
try = P.try

noneOf :: String -> Parser Char
noneOf = P.noneOf

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = (P.<|>)

(<..>) :: Char -> Char -> Parser Char
x <..> y = P.satisfy (\c -> c >= x && c <= y)

recognize :: Parser a -> Parser ()
recognize p = p >> return ()

stopOn :: [String] -> Parser String
stopOn list = many (try (noneOf hds <|> foldr1 (<|>) ps))
 where 
    (hds, ps) = unzip [ (x, make x xs) | x:xs <- list ]
    make x xs
       | null xs = fail "make"
       | otherwise = do 
            symbol x
            P.notFollowedBy (P.try (string xs >> return ' '))
            return x
      
symbol :: Char -> Parser ()
symbol c = P.char c >> return ()

string :: String -> Parser ()
string s = P.string s >> return ()

ranges :: [(Char, Char)] -> Parser Char
ranges xs = P.choice [ a <..> b | (a, b) <- xs ]

oneOf :: String -> Parser Char
oneOf = P.oneOf

many :: Parser a -> Parser [a] 
many = P.many

many1 :: Parser a -> Parser [a]
many1 = P.many1

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 = P.sepBy1

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 = P.chainr1

choice :: [Parser a] -> Parser a
choice = P.choice

optionM :: Parser a -> Parser (Maybe a)
optionM = P.optionMaybe

option :: a -> Parser a -> Parser a
option = P.option

doubleQuoted :: Parser a -> Parser a
doubleQuoted p = packed (symbol '"') p (symbol '"')

singleQuoted :: Parser a -> Parser a
singleQuoted p = packed (symbol '\'') p (symbol '\'')

parenthesized :: Parser a -> Parser a
parenthesized p = packed (symbol '(') p (symbol ')')
   
bracketed :: Parser a -> Parser a
bracketed p = packed (symbol '[') p (symbol ']')
   
packed :: Parser a -> Parser b -> Parser c -> Parser b
packed l p r = l >> p >>= \a -> r >> return a