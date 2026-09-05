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
-- Utility functions for parsing with Parsec library
--
-----------------------------------------------------------------------------

module Ideas.Utils.Parsing
   ( module Export
   , (<*>), (*>), (<*), (<$>), (<$), (<**>)
   , parseSimple, complete, skip, (<..>), ranges, stopOn
   , naturalOrFloat, float
   , UnbalancedError(..), balanced
   ) where

import Control.Applicative hiding ((<|>))
import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Text.ParserCombinators.Parsec as Export
import Text.ParserCombinators.Parsec.Expr as Export
import Text.ParserCombinators.Parsec.Language as Export
import Text.ParserCombinators.Parsec.Pos

parseSimple :: Parser a -> String -> Either String a
parseSimple p = left show . runParser (complete p) () ""

complete :: Parser a -> Parser a
complete p = spaces *> (p <* eof)

skip :: Parser a -> Parser ()
skip = void

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
      _         -> unexpected "not a float"
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
      _         -> unexpected "not a float"
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

-- simple function for finding unbalanced pairs (e.g. parentheses)
balanced :: [(Char, Char)] -> String -> Maybe UnbalancedError
balanced table = run (initialPos "") []
 where
   run _ [] [] = Nothing
   run _ ((pos, c):_) [] = return (NotClosed pos c)
   run pos stack (x:xs)
      | x `elem` opens  =
           run next ((pos, x):stack) xs
      | x `elem` closes =
           case stack of
              (_, y):rest | Just x == lookup y table -> run next rest xs
              _ -> return (NotOpened pos x)
      | otherwise =
           run next stack xs
    where
      next = updatePosChar pos x

   (opens, closes) = unzip table

data UnbalancedError = NotClosed SourcePos Char
                     | NotOpened SourcePos Char

instance Show UnbalancedError where
   show (NotClosed pos c) =
      show pos ++ ": Opening symbol " ++ [c] ++ " is not closed"
   show (NotOpened pos c) =
      show pos ++ ": Closing symbol " ++ [c] ++ " has no matching symbol"