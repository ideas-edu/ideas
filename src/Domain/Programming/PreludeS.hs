module Domain.Programming.PreludeS
   ( ModuleS, foldlS, letS, compS, etaS
   ) where

import Common.Context
import Common.Strategy
import Data.Maybe
import Domain.Programming.HeliumRules
import Domain.Programming.Helium

-- use applicative or monad type class for getting rules in to strategy context, and somehow get
-- the strategy arguments typed
-- pure = 

-- | Specialised strategies
type ModuleS = Strategy (Context Module)

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

-- specialised let strategy, also recognizes where clauses
letS :: Int -> ModuleS -> ModuleS -> ModuleS
letS ndecls exprS declS  =  introRHSExpr ndecls <*> exprS <*> declS
                        <|> introRHSExpr 0 <*> introExprLet ndecls <*> declS <*> exprS

compS :: ModuleS -> ModuleS -> ModuleS
compS f g  =  introExprInfixApplication True True <*> f <*> introExprVariable <*> introNameOperator "." <*> g
          <|> introExprLambda 1 <*> introPatternVariable <*> introNameIdentifier "aname" <*> -- not in free vars of f and g 
              introExprNormalApplication 1 <*> f <*> introExprParenthesized <*> introExprNormalApplication 1 <*>
              g <*> introExprVariable <*> introNameIdentifier "aname"

etaS :: ModuleS -> ModuleS
etaS expr  =  expr
          <|> introExprParenthesized <*> introExprLambda 1 <*> introPatternVariable <*> introNameIdentifier "x" <*> 
              introExprNormalApplication 1 <*> expr <*> introExprVariable <*> introNameIdentifier "x"

etaFunS :: ModuleS -> ModuleS -> ModuleS
etaFunS name expr =  introFunctionBindings 1 <*> introLHSFun 1 <*> name <*> introPatternVariable <*>
                     introNameIdentifier "x" <*> introRHSExpr 0 <*> introExprNormalApplication 1 <*> expr <*>
                     introExprVariable <*> introNameIdentifier "x"
                 <|> introPatternBinding <*> introPatternVariable <*> name <*> introRHSExpr 0 <*> etaS expr


{-
infixS :: Maybe ModuleS -> Maybe ModuleS -> ModuleS
infixS op l r  =  introExprInfixApplication (isJust l) (isJust r) <*>
                  fromMaybe succeed l <*> introExprVariable <*> introNameOperator op  <*>
                  fromMaybe succeed r
              <|> introExprLambda (f l + f r) <*> introPatternVariable <*> introNameIdentifier "aname" <*> -- not in free vars
                  introExprNormalApplication 1 <*> f <*> introExprParenthesized <*> introExprNormalApplication 1 <*>
                  g <*> introExprVariable <*> introNameIdentifier "aname"
  where
    f = maybe 0 (const 1)
-}

-- the insertion sort strategy with a fold (Johan)

{- the fold & para strategies
-- These strategy are only partially typed: the type of the arguments is
-- a strategy. Maybe we can obtain more type info in the arguments?
-- But I'm not sure if that is desirable.
--
-- Furthermore, we have to implement the different ways to construct a
-- foldr/para.
--
-- I think we need to define something like this in order to implement
-- different ways to implement folds.
-}


{-
foldS' :: Strategy (Context Expr) -> Strategy (Context Expr) -> Strategy (Context Expr)
foldS' consS nilS =   toStrategy (introVar "foldr") <*> consS <*> nilS
--                <|>  introLambda "aname" <*> introMatchList (Var "aname") 
--                                                            nilS 
--                                                            undefined
-- -- When recognising the lambda from te user I want to bind the string to a value which
-- -- I want to use later in the strategy.

paraS :: Strategy (Context Expr) -> Strategy (Context Expr) -> Strategy (Context Expr)
paraS consS nilS = toStrategy (introVar "para") <*> consS <*> nilS

-- Using fold and para

isortFoldStrategy :: Strategy (Context Expr)
isortFoldStrategy = foldS' insertS1 nilS -- '

insertS1 = getStrategy insertE
nilS = toStrategy (introVar "nil")

insertS2 = introLambda "a" <*> paraS insertConsS insertNilS
  where insertNilS =  getStrategy insertNilE
        insertConsS = getStrategy insertConsE

-- run = apply isortStrategy (inContext undef)

-- Bastiaan's code
type ExprS = Int -> Strategy (Context Expr)

list = map fromContext (applyAll (isortS 0) (inContext undef))
test = putStrLn $ unlines $ map (\e -> ppExpr (e,0)) list 
run  = eval (Apply (list!!0) mylist)
       
isortS :: ExprS
isortS = foldS insertS -- (\_ -> toStrategy $ introVar "insert") 
               (\_ -> toStrategy $ introVar "Nil")

insertS :: ExprS
insertS _ =
   rec <*> lam <*> lam <*> introMatchList <*> var 0 <*>
   appN 2 <*> focusTop <*> introVar "Cons" <*> var 1 <*> focusTop <*> introVar "Nil"
   <*> lam <*> lam <*> introIf <*> appN 2
   <*> focusTop <*> introVar "<=" <*> var 3 <*> var 1
   <*> appN 2 <*> focusTop <*> introVar "Cons" <*> var 3 <*> var 2
   <*> appN 2 <*> focusTop <*> introVar "Cons" <*> var 1 <*> appN 2
   <*> var 4 <*> var 3 <*> var 0 <*> focusTop
      
foldS :: ExprS -> ExprS -> ExprS
foldS consS nilS i
    =  appN 2 <*> introVar "foldr" <*> consS i <*> nilS i
   <|> lam <*> appN 3 <*> introVar "foldr" <*> consS (i+1) <*> nilS (i+1) <*> var 0
   <|> rec <*> lam <*> introMatchList <*> var 0 <*> focusTop <*> nilS (i+2) <*>
       lam <*> lam <*> appN 2 <*> focusTop <*> consS (i+4) <*> var 1 <*> 
       app <*> var 3 <*> var 0 

rec :: Strategy (Context Expr)
rec = focusTop <*> introFix <*> lam

lam :: Strategy (Context Expr)
lam = focusUndef <*> introLambdaAuto

var :: Int -> Strategy (Context Expr)
var n = focusUndef <*> introVarDB n

app :: Strategy (Context Expr)
app = focusTop <*> introApply

appN :: Int -> Strategy (Context Expr) 
appN 0 = succeed
appN n = app <*> appN (n-1)

-}