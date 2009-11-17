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
-- This module defines standard strategies based on Prelude functions
-- and Haskell language constructs.
-----------------------------------------------------------------------------

module Domain.Programming.PreludeS
   ( -- * Type synonyms
     ModuleS
     -- * Prelude strategies
   , foldlS, letS, whereS, compS, iterateS, sumS, zipWithS, reverseS
     -- * Smart constructors and help functions
   , varS, patS, progS, funS, declFunS, declPatS, rhsS, intS, appS, opS
   , lambdaS, mapSeqS, repeatS , ( # ), patConS, patParenS, exprParenS
   , patInfixConS, patWildcardS, exprConS
   ) where

import Common.Strategy hiding (repeat, replicate)
import Domain.Programming.HeliumRules
import Domain.Programming.Helium
import Prelude hiding (sequence)

--------------------------------------------------------------------------------
-- Type synonyms
--------------------------------------------------------------------------------
type ModuleS = Strategy Module
type ExprS   = Strategy Expression

--------------------------------------------------------------------------------
-- Language definition strategies
--------------------------------------------------------------------------------
whereS :: [ModuleS] -> ExprS -> ModuleS -- specialised let strategy, also recognizes where clauses
whereS decls expr  =  rhsS expr decls
                  <|> rhsS (letS decls expr) []

letS :: [ModuleS] -> ExprS -> ExprS
letS decls expr = introExprLet (length decls) <*> sequence decls <*> expr

--------------------------------------------------------------------------------
-- Prelude definition strategies
--------------------------------------------------------------------------------

foldlS :: ModuleS -> ModuleS -> ModuleS
foldlS consS nilS 
       -- Normal usage
    =  (varS "foldl" # [consS, nilS])
   <|> (varS "foldl'" # [consS, nilS])
       -- Foldl definition
   <|> letS [ declFunS [ funS "f" [ patS "nil", patConS "[]" ] (varS "nil") [] 
                       , funS "f" [ patS "nil", patParenS (patInfixConS (patS "x") ":" (patS "xs")) ]
                                  (varS "f" #  [ exprParenS (consS # [ varS "nil", varS "x" ]), varS "xs" ]) [] ]
            ] -- in
            ( varS "f" # [nilS] )
       -- Bastiaan's theorem, ie. foldl op e == foldr (flip op) e . reverse
   <|> compS (foldrS (flipS consS) nilS) (varS "reverse")

foldrS :: ModuleS -> ModuleS -> ModuleS
foldrS consS nilS  =  (varS "foldr" # [consS, nilS])
                  <|> letS [ declFunS [ funS "f" [ patConS "[]" ] nilS []
                                      , funS "f" [ patInfixConS (patS "x") ":" (patS "xs") ] 
                                                 (consS # [ varS "x", varS "f" # [ varS "xs" ] ] ) [] 
                                      ]
                           ] {- in -} ( varS "f" )

-- zie Hutton's paper voor nog een foldl def, ook inefficiente variant meenemen (foldl op e [] = ..)?

compS :: ModuleS -> ModuleS -> ModuleS -- f . g -> \x -> f (g x) 
compS f g  =  opS "." (Just f) (Just g)
          <|> lambdaS [patS "x"] (f # [appS g [varS "x"]])

flipS :: ModuleS -> ModuleS
flipS f  =  (varS "flip" # [f])
        <|> lambdaS [patS "x", patS "y"] (f # [varS "y", varS "x"])

sumS :: ModuleS
sumS =  varS "sum"  
--    <|> foldlS (opS "+" Nothing Nothing) (intS "0")

iterateS :: ModuleS
iterateS  =  varS "iterate"
         <|> letS [ declFunS [ funS "f" [ patS "g", patS "x" ] 
                                     (opS ":" (Just (varS "x"))
                                              (Just (varS "f" # [varS "g", varS "g" # [varS "x"]]))) []] 
                  ] ( varS "f" )

zipWithS :: ModuleS
zipWithS = varS "zipWith"
{-
zipWith :: (a->b->c) -> [a]->[b]->[c]
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
zipWith _ _      _      = []
-}

reverseS :: ModuleS
reverseS  =  (varS "reverse")
--         <|> foldlS (flipS (opS ":" Nothing Nothing)) (exprConS "[]")

--------------------------------------------------------------------------------
-- Smart constructors
--------------------------------------------------------------------------------
varS :: String -> ModuleS
varS n = introExprVariable <*> introNameIdentifier n

exprConS :: String -> ModuleS
exprConS n = introExprConstructor <*> introNameSpecial n 

exprParenS = (introExprParenthesized <*>)

patS :: String -> ModuleS
patS n = introPatternVariable <*> introNameIdentifier n

patWildcardS :: ModuleS
patWildcardS = toStrategy introPatternWildcard

patConS :: String -> ModuleS
patConS n = introPatternConstructor 0 <*> introNameSpecial n

patParenS = (introPatternParenthesized <*>)

patInfixConS :: ModuleS -> String -> ModuleS -> ModuleS
patInfixConS l con r = introPatternInfixConstructor <*> l <*> 
                       introNameSpecial con <*> r

funS :: String -> [ModuleS] -> ExprS -> [ModuleS] -> ModuleS
funS name args rhs ws  =  introLHSFun (length args)
                      <*> introNameIdentifier name <*> sequence args 
                      <*> rhsS rhs ws

declFunS :: [ModuleS] -> ModuleS
declFunS fs = introFunctionBindings (length fs) <*> sequence fs -- also recognise patbinding/lambda?

declPatS :: String -> ExprS -> [ModuleS] -> ModuleS
declPatS name rhs ws = introPatternBinding <*> patS name <*> rhsS rhs ws

rhsS :: ExprS -> [ModuleS] -> ModuleS
rhsS expr ws  =  introRHSExpr (length ws) <*> liftStrategy expr 
             <*> if null ws then succeed else sequence ws -- where clause

intS :: String -> ExprS
intS i = introExprLiteral <*> liftRule (introLiteralInt i)

progS :: [ModuleS] -> ModuleS
progS decls = introModule <*> introDecls (length decls) <*> sequence decls

appS :: ModuleS -> [ModuleS] -> ModuleS
appS f as  =  introExprNormalApplication (length as) <*> f <*> sequence as

infixr 0 #
( # ) = appS

opS :: String -> Maybe ModuleS -> Maybe ModuleS -> ModuleS
opS n l r = case (l, r) of 
              (Just x, Just y)   -> infixApp True True   <*> x  <*> op <*> y
              (Nothing, Just y)  -> infixApp False True  <*> op <*> y
              (Just x, Nothing)  -> infixApp True False  <*> x  <*> op
              (Nothing, Nothing) -> infixApp False False <*> op
  where 
    infixApp = introExprInfixApplication
    op = introExprVariable <*> introNameOperator n

lambdaS :: [ModuleS] -> ModuleS -> ModuleS
lambdaS as expr = introExprLambda (length as) <*> sequence as <*> expr


--------------------------------------------------------------------------------
-- Help functions
--------------------------------------------------------------------------------
mapSeqS f = sequence . map f
repeatS n = sequence . replicate n
liftStrategy = mapRulesS liftRule
