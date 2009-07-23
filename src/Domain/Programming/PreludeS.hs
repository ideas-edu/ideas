module Domain.Programming.PreludeS
{-   ( ModuleS, foldlS, letS, compS, etaS
   )-} where

import Common.Context hiding (Var)
import Common.Strategy hiding (repeat)
import Data.List
import Data.Maybe
import Domain.Programming.HeliumRules
import Domain.Programming.Helium
import Prelude hiding (fail)

{- ideas:

   o  use applicative or monad type class for getting rules in to strategy 
      context

   o  Use a GADT to get the strategies typed again
-}

--------------------------------------------------------------------------------
-- Type synonyms
--------------------------------------------------------------------------------
type ModuleS = Strategy (Context Module)
type Var = String
type Pat = String

--------------------------------------------------------------------------------
-- Language definition strategies
--------------------------------------------------------------------------------
letS :: Int -> ModuleS -> ModuleS -> ModuleS -- specialised let strategy, also recognizes where clauses
letS ndecls expr decl  =  introRHSExpr ndecls <*> expr <*> decl
                      <|> introRHSExpr 0 <*> introExprLet ndecls 
                                         <*> decl <*> expr

f # args = appS f args
infixr 0 #

appS :: ModuleS -> [ModuleS] -> ModuleS
appS f args = curryS f args <|> infixS f args

infixS f args = case args of 
                  x:y:z:zs -> app (parenS (infixApp f x y)) (z:zs)
                  x:y:[]   -> infixApp f x y
                  _        -> fail
  where
    infixApp f l r = introExprInfixApplication True True <*> l <*> f <*> r

curryS :: ModuleS -> [ModuleS] -> ModuleS
curryS f args  = case args of 
                   []   -> fail
                   a:as -> app f (a:as) <|> partialS f args <|> curryS (parenS (app f [a])) as

partialS f args | length args > 2  = choiceS $ map (\(x, y) -> curryS (parenS (f `app` x)) y) (split args)
                | otherwise        = fail

app f as = introExprNormalApplication (length as) <*> f <*> seqS as

opS :: String -> Maybe ModuleS -> Maybe ModuleS -> ModuleS
opS n l r = case (l, r) of 
              (Just x, Just y)   -> app (introExprInfixApplication False False <*> op) [x, y]    <|> 
                                    app (introExprInfixApplication True  False <*> x  <*> op) [y] <|> 
                                    app (introExprInfixApplication False True  <*> op <*>  y) [x] <|> 
                                    introExprInfixApplication True True   <*> x  <*> op <*> y
              (Nothing, Just y)  -> introExprInfixApplication False True  <*> op <*> y
              (Just x, Nothing)  -> introExprInfixApplication True  False <*> x  <*> op
              (Nothing, Nothing) -> introExprInfixApplication False False <*> op         
  where 
    op = introExprVariable <*> introNameOperator n

etaS :: ModuleS -> ModuleS -- f => \x -> f x
etaS expr = expr <|> parenS (lambdaS arg (appS expr arg))
  where arg = [varS "x"]

etaFunS :: ModuleS -> ModuleS -> ModuleS
etaFunS name expr =  introFunctionBindings 1 <*> introLHSFun 1 <*> name <*> introPatternVariable <*>
                     introNameIdentifier "x" <*> introRHSExpr 0 <*> introExprNormalApplication 1 <*> expr <*>
                     introExprVariable <*> introNameIdentifier "x"
                 <|> introPatternBinding <*> introPatternVariable <*> name <*> introRHSExpr 0 <*> etaS expr

lambdaS :: [ModuleS] -> ModuleS -> ModuleS -- do via context
lambdaS ps expr  =  introExprLambda (length ps) <*> seqS ps <*> expr
                <|> seqS (map (\x -> introExprLambda 1 <*> x) ps) <*> expr


--------------------------------------------------------------------------------
-- Prelude definition strategies
--------------------------------------------------------------------------------

foldlS :: ModuleS -> ModuleS -> ModuleS
foldlS consS nilS 
    =  introRHSExpr 0 <*> introExprNormalApplication 2 <*> introExprVariable <*> introNameIdentifier "foldl" 
                                                       <*> consS <*> nilS 
   <|> letS 1 ( introExprNormalApplication 1 <*> introExprVariable <*> introNameIdentifier "f" <*> nilS )
              ( introFunctionBindings 2
                  <*> introLHSFun 2 <*> introNameIdentifier "f"
                                    <*> introPatternVariable <*> introNameIdentifier "nil"
                                    <*> introPatternConstructor 0 <*> introNameSpecial "[]"
                      <*> introRHSExpr 0 
                                    <*> introExprVariable <*> introNameIdentifier "nil"
                  <*> introLHSFun 2 <*> introNameIdentifier "f"
                                    <*> introPatternVariable <*> introNameIdentifier "nil"
                                    <*> introPatternParenthesized 
                                    <*> introPatternInfixConstructor 
                                    <*> introPatternVariable <*> introNameIdentifier "x"
                                    <*> introNameSpecial ":"
                                    <*> introPatternVariable <*> introNameIdentifier "xs"
                      <*> introRHSExpr 0 
                                    <*> introExprNormalApplication 2 
                                    <*> introExprVariable <*> introNameIdentifier "f"
                                    <*> introExprParenthesized 
                                    <*> introExprNormalApplication 2
                                    <*> consS
                                    <*> introExprVariable <*> introNameIdentifier "nil"
                                    <*> introExprVariable <*> introNameIdentifier "x"
                                    <*> introExprVariable <*> introNameIdentifier "xs"
              )


-- composition is nothing different than infix app
compS :: ModuleS -> ModuleS -> ModuleS -- f . g -> \x -> f (g x) 
compS f g  =  introExprInfixApplication True True <*> f <*> opS "." <*> g
          <|> lambdaS [patS "x"] (appS f [parenS (appS g [varS "x"])]) -- not in free vars of f and g 
  where opS n = introExprVariable <*> introNameOperator n

--------------------------------------------------------------------------------
-- Smart constructors
--------------------------------------------------------------------------------
varS :: String -> ModuleS
varS n = introExprVariable <*> introNameIdentifier n

patS :: String -> ModuleS
patS n = introPatternVariable <*> introNameIdentifier n

parenS :: ModuleS -> ModuleS
parenS expr = introExprParenthesized <*> expr

funS :: String -> [ModuleS] -> ModuleS -> [ModuleS] -> ModuleS
funS name args rhs ws  =  introLHSFun (length args)
                      <*> introNameIdentifier name <*> seqS args 
                      <*> rhsS rhs ws

declFunS :: [ModuleS] -> ModuleS
declFunS fs = introFunctionBindings (length fs) <*> seqS fs

declPatS :: String -> ModuleS -> [ModuleS] -> ModuleS
declPatS name rhs ws = introPatternBinding <*> patS name <*> rhsS rhs ws

rhsS :: ModuleS -> [ModuleS] -> ModuleS
rhsS expr ws  =  introRHSExpr (length ws) <*> expr 
             <*> if null ws then succeed else seqS ws -- where clause

intS :: String -> ModuleS
intS i = introExprLiteral <*> introLiteralInt i

infixFunS = undefined

--------------------------------------------------------------------------------
-- Help functions
--------------------------------------------------------------------------------
seqS = foldr1 (<*>)
choiceS = foldr1 (<|>)
mapSeqS f = seqS . (map f)
repeatS n = seqS . (take n) . repeat

split xs = init $ map (\x -> splitAt x xs) [1..length xs]
