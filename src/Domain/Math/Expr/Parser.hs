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

import Common.Algebra.Boolean (BoolValue, Boolean(..), ands)
import Common.Classes
import Common.Id
import Common.Rewriting
import Common.Transformation
import Control.Monad
import Data.Monoid
import Domain.Logic.Formula (Logic, catLogic)
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Expr.Data
import Domain.Math.Expr.Symbols
import Prelude hiding ((^))
import Text.Parsing
import Test.QuickCheck (arbitrary)
import qualified Text.ParserCombinators.Parsec.Token as P

parseExpr :: String -> Either String Expr
parseExpr = parseSimple expr

parseEqExpr :: String -> Either String (Equation Expr)
parseEqExpr = parseSimple (equation expr)

parseRelExpr :: String -> Either String (Relation Expr)
parseRelExpr = parseSimple (relation expr)
   
parseOrsEqExpr :: String -> Either String (OrList (Equation Expr))
parseOrsEqExpr = parseSimple (ors (equation expr))

parseOrsRelExpr :: String -> Either String (OrList (Relation Expr))
parseOrsRelExpr = parseSimple (ors (relation expr))

parseLogicRelExpr :: String -> Either String (Logic (Relation Expr))
parseLogicRelExpr = parseSimple (catLogic <$> logic (relationChain expr))

parseExprTuple :: String -> Either String [Expr]
parseExprTuple = parseSimple (tuple expr)

ors :: Parser a -> Parser (OrList a)
ors p = mconcat <$> sepBy1 (logicAtom p) (reserved "or")

logic :: Parser a -> Parser (Logic a)
logic p = buildExpressionParser table (logicAtom p)
 where
   table = 
      [ [Infix ((<&&>) <$ reservedOp "and") AssocRight] 
      , [Infix ((<||>) <$ reservedOp "or" ) AssocRight] 
      ]

logicAtom :: (Container f, BoolValue (f a)) => Parser a -> Parser (f a)
logicAtom p = choice 
   [ true  <$  reserved "true" 
   , false <$  reserved "false"
   , to    <$> p
   ]

equation :: Parser a -> Parser (Equation a)
equation p = (:==:) <$> p <* reservedOp "==" <*> p

relation :: Parser a -> Parser (Relation a)
relation p = p <**> relType <*> p

relationChain :: Parser a -> Parser (Logic (Relation a))
relationChain p = (\x -> ands . make x) <$> p <*> many1 ((,) <$> relType <*> p)
 where
   make _ []             = []
   make a ((f, b): rest) = to (f a b) : make b rest

relType :: Parser (a -> a -> Relation a)
relType = choice (map make table)
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
   [ sqrt <$ reserved "sqrt" <*> atom
   , binary rootSymbol <$ reserved "root" <*> atom <*> atom
   , binary logSymbol  <$ reserved "log"  <*> atom <*> atom
   , do reserved "D"
        x <- identifier <|> parens identifier
        a <- atom
        return $ unary diffSymbol (binary lambdaSymbol (Var x) a)
   , do a  <- qualId
        as <- many atom
        return (function (newSymbol a) as)
   , atom
   ] 
      
atom :: Parser Expr
atom = choice 
   [ try (fromDouble <$> float)
   , fromInteger <$> integer
   , Var <$> identifier 
   , parens expr
   ]

exprTable :: [[Operator Char () Expr]]
exprTable = 
   [ -- precedence level 7
     [ Infix ((^) <$ reservedOp "^") AssocRight
     ]
     -- precedence level 7
   , [ Infix ((*) <$ reservedOp "*") AssocLeft
     , Infix ((/) <$ reservedOp "/") AssocLeft
     ]
     -- precedence level 6+
   , [ Prefix (negate <$ reservedOp "-") 
     ] 
     -- precedence level 6
   , [ Infix ((+) <$ reservedOp "+") AssocLeft
     , Infix ((-) <$ reservedOp "-") AssocLeft
     ]
   ]

--------------------------------------------------------------------------
-- Lexing

lexer :: P.TokenParser a
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

qualId :: CharParser st Id
qualId = try (P.lexeme lexer (do
   xs <- idPart `sepBy1` char '.'
   guard (length xs > 1)
   return (mconcat (map newId xs))) 
 <?> "qualified identifier")
 where
   idPart   = (:) <$> letter <*> many idLetter
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
exprArgDescr descr = 
   let p = either (const Nothing) Just . parseExpr
   in ArgDescr descr Nothing p show arbitrary