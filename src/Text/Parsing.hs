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
   , parseSimple
   ) where
   
import Control.Arrow
import Control.Applicative
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

parseSimple :: Parser a -> String -> Either String a
parseSimple p = left show . runParser (spaces *> (p <* eof)) () ""