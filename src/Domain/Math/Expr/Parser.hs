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
-----------------------------------------------------------------------------
module Domain.Math.Expr.Parser
   ( parseExpr
   , parseEqExpr, parseRelExpr
   , parseOrsEqExpr, parseOrsRelExpr
   , parseLogicRelExpr
   , parseExprTuple
   ) where

import Prelude hiding ((^))
import Domain.Math.Expr.Symbols
import Domain.Math.Data.Relation
import Control.Arrow
import Control.Monad
import Common.Classes
import Common.Algebra.Boolean (BoolValue)
import Common.Id
import Common.Rewriting
import Data.Monoid
import Domain.Math.Expr.Data
import Domain.Math.Data.OrList
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token (TokenParser)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Char
import Domain.Logic.Formula (Logic)
import qualified Domain.Logic.Formula as Logic
import Common.Transformation
import Test.QuickCheck (arbitrary)

parseWith :: Parser a -> String -> Either String a
parseWith p = left show . runParser start () ""
 where
   start = (P.whiteSpace lexer) >> p >>= \a -> eof >> return a

parseExpr :: String -> Either String Expr
parseExpr = parseWith expr

parseEqExpr :: String -> Either String (Equation Expr)
parseEqExpr = parseWith (equation expr)

parseRelExpr :: String -> Either String (Relation Expr)
parseRelExpr = parseWith (relation expr)
   
parseOrsEqExpr :: String -> Either String (OrList (Equation Expr))
parseOrsEqExpr = parseWith (ors (equation expr))

parseOrsRelExpr :: String -> Either String (OrList (Relation Expr))
parseOrsRelExpr = parseWith (ors (relation expr))

parseLogicRelExpr :: String -> Either String (Logic (Relation Expr))
parseLogicRelExpr = parseWith (logic (relation expr))

parseExprTuple :: String -> Either String [Expr]
parseExprTuple = parseWith (tuple expr)

ors :: Parser a -> Parser (OrList a)
ors p = do
   xs <- sepBy1 (logicAtom p) (reserved "or")
   return (mconcat xs)

logic :: Parser a -> Parser (Logic a)
logic p = buildExpressionParser table (logicAtom p)
 where
   table = 
      [ [Infix (reservedOp "and" >> return (Logic.:&&:)) AssocRight] 
      , [Infix (reservedOp "or"  >> return (Logic.:||:)) AssocRight] 
      ]

logicAtom :: (Container f, BoolValue (f a)) => Parser a -> Parser (f a)
logicAtom p = choice 
   [ reserved "true"  >> return true
   , reserved "false" >> return false
   , liftM to p
   ]

equation :: Parser a -> Parser (Equation a)
equation p = do
   a <- p
   reservedOp "=="
   b <- p
   return (a :==: b)
      
relation :: Parser a -> Parser (Relation a)
relation p = do
   a <- p
   f <- choice (map make table)
   b <- p
   return (f a b)
 where
   make (s, f) = reserved s >> return f
   table = 
      [ ("==", (.==.)), ("<=", (.<=.)), (">=", (.>=.))
      , ("<", (.<.)), (">", (.>.)), ("~=", (.~=.))
      ]

tuple :: Parser a -> Parser [a]
tuple p = parens (sepBy p comma)

expr :: Parser Expr
expr = buildExpressionParser exprTable term 

term :: Parser Expr
term = choice 
   [ reserved "sqrt" >> liftM sqrt atom
   , reserved "root" >> liftM2 (binary rootSymbol) atom atom
   , reserved "log"  >> liftM2 (binary logSymbol) atom atom
   , do reserved "D"
        x <- identifier <|> parens identifier
        a <- atom
        return $ unary diffSymbol (binary lambdaSymbol (Var x) a)
   , do a  <- qualified
        as <- many atom
        return (function (newSymbol a) as)
   , atom
   ] 
      
atom :: Parser Expr
atom = choice 
   [ try (liftM Number float)
   , liftM fromInteger integer
   , liftM Var identifier 
   , parens expr
   ]

exprTable :: [[Operator Char () Expr]]
exprTable = 
   [ -- precedence level 7
     [ Infix (reservedOp "^" >> return (^)) AssocRight
     ]
     -- precedence level 7
   , [ Infix (reservedOp "*" >> return (*)) AssocLeft
     , Infix (reservedOp "/" >> return (/)) AssocLeft
     ]
     -- precedence level 6+
   , [ Prefix (reservedOp "-" >> return negate) 
     ] 
     -- precedence level 6
   , [ Infix (reservedOp "+" >> return (+)) AssocLeft
     , Infix (reservedOp "-" >> return (-)) AssocLeft
     ]
   ]

--------------------------------------------------------------------------
-- Lexing

lexer :: TokenParser a
lexer = P.makeTokenParser $ emptyDef 
   { reservedNames   = ["sqrt", "root", "log", "and", "or", "true", "false", "D"]
   , reservedOpNames = ["==", "<=", ">=", "<", ">", "~=", "+", "-", "*", "^", "/"]
   }

integer :: Parser Integer
integer = P.integer lexer

float :: Parser Double
float = P.float lexer

identifier :: Parser String
identifier = P.identifier lexer

qualified :: CharParser st Id
qualified = try (P.lexeme lexer (do
   xs <- idPart `sepBy1` char '.'
   guard (length xs > 1)
   return (mconcat (map newId xs))) 
 <?> "qualified identifier")
 where
   idPart   = many1 idLetter
   idLetter = alphaNum <|> oneOf "-_"

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

comma :: Parser String
comma = P.comma lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

-----------------------------------------------------------------------
-- Argument descriptor (for parameterized rules)

instance Argument Expr where
   makeArgDescr = exprArgDescr

exprArgDescr :: String -> ArgDescr Expr
exprArgDescr descr = ArgDescr descr Nothing (either (const Nothing) Just . parseExpr) show arbitrary