-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  unknown
--
--
-----------------------------------------------------------------------------

module Domain.Programming.PreludeS
   ( -- * Type synonyms
     ModuleS
     -- * Prelude strategies
   , foldlS, letS, compS
     -- * Smart constructors and help functions
   , varS, patS, modS, funS, declFunS, declPatS, rhsS, intS, appS, opS
   , lambdaS, mapSeqS, repeatS , ( # ), patConS, patParenS, exprParenS
   , patInfixConS
   ) where

import Common.Context hiding (Var)
import Common.Strategy hiding (repeat)
import Domain.Programming.HeliumRules
import Domain.Programming.Helium
import Prelude hiding (sequence)

--------------------------------------------------------------------------------
-- Type synonyms
--------------------------------------------------------------------------------
type ModuleS = Strategy (Context Module)


--------------------------------------------------------------------------------
-- Language definition strategies
--------------------------------------------------------------------------------
whereS :: [ModuleS] -> ModuleS -> ModuleS -- specialised let strategy, also recognizes where clauses
whereS decls expr  =  rhsS expr decls
                  <|> rhsS (letS decls expr) []

letS :: [ModuleS] -> ModuleS -> ModuleS
letS decls expr = introExprLet (length decls) <*> sequence decls <*> expr

--------------------------------------------------------------------------------
-- Prelude definition strategies
--------------------------------------------------------------------------------

foldlS :: ModuleS -> ModuleS -> ModuleS
foldlS consS nilS 
       -- Normal usage
    = {- (varS "foldl" # [consS, nilS])
       -- Foldl definition
   <|> letS [ declFunS [ funS "f" [ patS "nil", patConS "[]" ] (varS "nil") [] 
                       , funS "f" [ patS "nil", patParenS (patInfixConS (patS "x") ":" (patS "xs")) ]
                                  (varS "f" #  [ exprParenS (consS # [ varS "nil", varS "x" ]), varS "xs" ]) [] ]
            ] -- in
            ( varS "f" # [nilS] )
       -- Bastiaan's theorem, ie. foldl op e == foldr (flip op) e . reverse
   <|> -}compS (varS "foldr" # [flipS consS, nilS]) (varS "reverse")

-- zie Hutton's paper voor nog een foldl def, ook inefficiente variant meenemen (foldl op e [] = ..)?

compS :: ModuleS -> ModuleS -> ModuleS -- f . g -> \x -> f (g x) 
compS f g  =  opS "." (Just f) (Just g)
          <|> lambdaS [patS "x"] (appS f [appS g [varS "x"]])

flipS :: ModuleS -> ModuleS
flipS f  =  (varS "flip" # [f])
        <|> lambdaS [patS "x", patS "y"] (f # [varS "y", varS "x"])


--------------------------------------------------------------------------------
-- Smart constructors
--------------------------------------------------------------------------------
varS :: String -> ModuleS
varS n = introExprVariable <*> introNameIdentifier n

exprParenS expr = introExprParenthesized <*> expr

patS :: String -> ModuleS
patS n = introPatternVariable <*> introNameIdentifier n

patConS :: String -> ModuleS
patConS n = introPatternConstructor 0 <*> introNameSpecial n

patParenS expr = introPatternParenthesized <*> expr

patInfixConS :: ModuleS -> String -> ModuleS -> ModuleS
patInfixConS l con r = introPatternInfixConstructor <*> l <*> 
                       introNameSpecial con <*> r

funS :: String -> [ModuleS] -> ModuleS -> [ModuleS] -> ModuleS
funS name args rhs ws  =  introLHSFun (length args)
                      <*> introNameIdentifier name <*> sequence args 
                      <*> rhsS rhs ws

declFunS :: [ModuleS] -> ModuleS
declFunS fs = introFunctionBindings (length fs) <*> sequence fs

declPatS :: String -> ModuleS -> [ModuleS] -> ModuleS
declPatS name rhs ws = introPatternBinding <*> patS name <*> rhsS rhs ws

rhsS :: ModuleS -> [ModuleS] -> ModuleS
rhsS expr ws  =  introRHSExpr (length ws) <*> expr 
             <*> if null ws then succeed else sequence ws -- where clause

intS :: String -> ModuleS
intS i = introExprLiteral <*> introLiteralInt i

modS :: [ModuleS] -> ModuleS
modS decls = introModule <*> introDecls (length decls) <*> sequence decls

appS :: ModuleS -> [ModuleS] -> ModuleS
appS f as = introExprNormalApplication (length as) <*> f <*> sequence as

infixr 0 #
f # as = appS f as

opS :: String -> Maybe ModuleS -> Maybe ModuleS -> ModuleS
opS n l r = case (l, r) of 
              (Just x, Just y)   -> infixApp True True   <*> x  <*> op <*> y
              (Nothing, Just y)  -> infixApp False True  <*> op <*> y
              (Just x, Nothing)  -> infixApp True False  <*> x  <*> op
              (Nothing, Nothing) -> infixApp False False <*> op
  where 
    infixApp l r = introExprInfixApplication l r
    op = introExprVariable <*> introNameOperator n

lambdaS :: [ModuleS] -> ModuleS -> ModuleS
lambdaS as expr = introExprLambda (length as) <*> sequence as <*> expr


--------------------------------------------------------------------------------
-- Help functions
--------------------------------------------------------------------------------
mapSeqS f = sequence . (map f)
repeatS n = sequence . (take n) . repeat

