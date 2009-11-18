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
{-   ( -- * Type synonyms
     ModuleS
     -- * Prelude strategies
   , foldlS, letS, whereS, compS, iterateS, sumS, zipWithS, reverseS
     -- * Smart constructors and help functions
   , varS, patS, progS, funS, declFunS, declPatS, rhsS, intS, appS, opS
   , lambdaS, mapSeqS, repeatS , ( # ), patConS 
   , patInfixConS, patWildcardS, exprConS
   ) -} where

import Common.Strategy hiding (repeat, replicate)
import Data.Data
import Domain.Programming.HeliumRules
import Domain.Programming.Helium
import Prelude hiding (sequence)

--------------------------------------------------------------------------------
-- Type synonyms
--------------------------------------------------------------------------------
type ModuleS     = Strategy Module
type ExprS       = Strategy Expression
type PatS        = Strategy Pattern
type FunBindingS = Strategy FunctionBinding
type RhsS        = Strategy RightHandSide
type LhsS        = Strategy LeftHandSide
type DeclS       = Strategy Declaration

--------------------------------------------------------------------------------
-- Language definition strategies
--------------------------------------------------------------------------------
-- specialised where strategy, also recognizes let expressions
whereS :: [DeclS] -> ExprS -> RhsS 
whereS decls expr  =  rhsS expr decls
                  <|> rhsS (letS decls expr) []

letS :: [DeclS] -> ExprS -> ExprS
letS decls expr = introExprLet (length decls) <**> sequence decls <*> expr

------------------------------------------------------------------------------
-- Prelude definition strategies
------------------------------------------------------------------------------
foldlS :: ExprS -> ExprS -> ExprS
foldlS consS nilS 
       -- Normal usage
    =  (varS "foldl" # [consS, nilS])
   <|> (varS "foldl'" # [consS, nilS])
       -- Foldl definition
   <|> letS [ declFunS [ funS (lhsS "f" [patS "nil", patConS "[]"]) (rhsS (varS "nil") [])
                       , funS (lhsS "f" [patS "nil", patParenS (patInfixConS (patS "x") ":" (patS "xs"))])
                              (rhsS (varS "f" #  [ exprParenS (consS # [ varS "nil", varS "x" ]), varS "xs" ]) [])
                       ]
            ] -- in
            (varS "f" # [nilS])
       -- Bastiaan's theorem, ie. foldl op e == foldr (flip op) e . reverse
   <|> compS (foldrS (flipS consS) nilS) (varS "reverse")

foldrS :: ExprS -> ExprS -> ExprS
foldrS consS nilS  =  (varS "foldr" # [consS, nilS])
                  <|> letS [ declFunS [ funS (lhsS "f" [ patConS "[]" ]) (rhsS nilS [])
                                      , funS (lhsS "f" [ patInfixConS (patS "x") ":" (patS "xs") ]) 
                                             (rhsS (consS # [ varS "x", varS "f" # [ varS "xs" ] ]) []) 
                                      ]
                           ] {- in -} (varS "f")

compS :: ExprS -> ExprS -> ExprS -- f . g -> \x -> f (g x) 
compS f g  =  opS "." (Just f) (Just g)
          <|> lambdaS [patS "x"] (f # [appS g [varS "x"]])

flipS :: ExprS -> ExprS
flipS f  =  (varS "flip" # [f])
        <|> lambdaS [patS "x", patS "y"] (f # [varS "y", varS "x"])

sumS :: ExprS
sumS =  varS "sum"  
--    <|> foldlS (opS "+" Nothing Nothing) (intS "0")

iterateS :: ExprS
iterateS  =  varS "iterate"

zipWithS :: ExprS
zipWithS = varS "zipWith"
{-
zipWith :: (a->b->c) -> [a]->[b]->[c]
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
zipWith _ _      _      = []
-}

reverseS :: ExprS
reverseS  =  (varS "reverse")
--         <|> foldlS (flipS (opS ":" Nothing Nothing)) (exprConS "[]")

--------------------------------------------------------------------------------
-- Smart constructors
--------------------------------------------------------------------------------
varS :: String -> ExprS
varS n = introExprVariable <***> introNameIdentifier n

exprConS :: String -> ExprS
exprConS n = introExprConstructor <***> introNameSpecial n

exprParenS :: ExprS -> ExprS
exprParenS = (introExprParenthesized <*>)

patS :: String -> PatS
patS n = introPatternVariable <***> introNameIdentifier n

patParenS :: PatS -> PatS
patParenS = (introPatternParenthesized <*>)

patWildcardS :: PatS
patWildcardS = toStrategy introPatternWildcard

patConS :: String -> PatS
patConS n = introPatternConstructor 0 <***> introNameSpecial n

patInfixConS :: PatS -> String -> PatS -> PatS
patInfixConS l con r = (introPatternInfixConstructor <*> l <***> 
                        introNameSpecial con) <*> r

funS :: LhsS -> RhsS -> FunBindingS
funS lhs rhs = introFunBinding <**> lhs <**> rhs

declFunS :: [FunBindingS] -> DeclS
declFunS fbs = introDeclFunBindings (length fbs) <**> sequence fbs

declPatS :: String -> RhsS -> DeclS
declPatS name rhs = introDeclPatBinding <**> patS name <**> rhs

lhsS :: String -> [PatS] -> LhsS
lhsS name args =   introLHSFun (length args) <***> introNameIdentifier name 
              <**> sequence args 

rhsS :: ExprS -> [DeclS] -> RhsS
rhsS expr ws = introRHSExpr (length ws) <**> expr <**> sequence ws

intS :: String -> ExprS
intS i = introExprLiteral <***> introLiteralInt i

progS :: [DeclS] -> ModuleS
progS decls = introModule <***> introBody (length decls) <**> sequence decls

appS :: ExprS -> [ExprS] -> ExprS
appS f as = introExprNormalApplication (length as) <*> f <*> sequence as

infixr 0 #
( # ) = appS

opS :: String -> Maybe ExprS -> Maybe ExprS -> ExprS
opS n l r = case (l, r) of 
              (Just x, Just y)   -> infixApp True True   <*> x  <*> op <*> y
              (Nothing, Just y)  -> infixApp False True  <*> op <*> y
              (Just x, Nothing)  -> infixApp True False  <*> x  <*> op
              (Nothing, Nothing) -> infixApp False False <*> op
  where 
    infixApp = introExprInfixApplication
    op       = introExprVariable <***> introNameOperator n

lambdaS :: [PatS] -> ExprS -> ExprS
lambdaS args expr = introExprLambda (length args) <**> sequence args <*> expr


--------------------------------------------------------------------------------
-- Help functions
--------------------------------------------------------------------------------
mapSeqS f = sequence . map f
repeatS n = sequence . replicate n

liftStrategy :: (Data a, Data b, Eq a, Undefined a) => Strategy a -> Strategy b
liftStrategy s = mapRulesS liftRule s

r <**> q = r <*> liftStrategy q
infixr 6 <**>

r <***> q = r <*> liftRule q
infixr 7 <***>

uncurry3 f t = f ((\(a, _, _) -> a) t) 
                 ((\(_, b, _) -> b) t) 
                 ((\(_, _, c) -> c) t)
