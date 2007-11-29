{----------------------------------------------------------------
This module contains a temporary parser for proposistion formulae

Copyright (c)        2006 - 2007 
Johan Jeuring and Harrie Passier
---------------------------------------------------}


module LogicParser where

-- Standard Haskell libraries
import Char
import List (intersperse)

-- GHC library
import GHC.Real
import Debug.Trace

-- UU libraries
import ParserComb 
import UU.Pretty
import LogicFormula


-- Read and parse a formula 
parseFormula :: String -> Formula 
parseFormula  = fst . safehead . filter (\(x,y) -> y == "") . formula . filter (/=' ')
               where
               safehead xs = if length xs > 0
                             then head xs
                             else error "safehead readformula" 

{-------------------------------------------------------------------------
 Logic parser : formula
 The parser reads a string according to a concrete syntax
 and produces an abstract syntax
-------------------------------------------------------------------------}
 
formula     = chainr disjunction (const (:<->:) <$> eqvSign) 
disjunction = chainr conjunction (const (:||:)  <$> orSign ) 
conjunction = chainr implication (const (:&&:)  <$> andSign)
implication = chainr basic       (const (:->:)  <$> impSign)


basic =  Var <$> identifier
     <|> parenthesised formula
     <|> const T <$> symbol 'T'
     <|> const F <$> symbol 'F'
     <|> nott


nott :: Parser Char Formula
nott = (\a b -> Not b) <$> notSign <*> formula 



andSign = token "/\\"
orSign  = token "||" 
impSign = token "->"
eqvSign = token "<->"
notSign = token "~"


