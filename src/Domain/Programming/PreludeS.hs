module Domain.Programming.PreludeS
{-   ( ModuleS, foldlS, letS, compS, etaS
   )-} where

import Common.Context hiding (Var)
import Common.Strategy hiding (repeat)
import Domain.Programming.HeliumRules
import Domain.Programming.Helium
import Prelude hiding (fail, sequence)

{- ideas:

   o  use applicative or monad type class for getting rules in to strategy 
      context

   o  Use a GADT to get the strategies typed again

   o  Change show function for names, show also the name of the constructor.
      For the range datatype: remove from show.

   o  Devise a scheme in which certain strategies, like etaS and parenS, can
      be used at any point in the strategy.

   o  Add beta reduction, uniplate for app (\x->expr) y

   o  Add etaS to prelude strategies
-}

--------------------------------------------------------------------------------
-- Type synonyms
--------------------------------------------------------------------------------
type ModuleS = Strategy (Context Module)

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
appS f args = etaS $ curryS f args <|> infixS f args

infixS f args = case args of 
                  x:y:z:zs -> app (parenS (infixApp f x y)) (z:zs)
                  x:y:[]   -> infixApp f x y
                  _        -> fail
  where
    infixApp f l r = introExprInfixApplication True True <*> l <*> f <*> r

curryS :: ModuleS -> [ModuleS] -> ModuleS
curryS f args  = case args of 
                   []   -> fail
                   a:as -> app f (a:as) <|> partialS f args 
                                        <|> curryS (parenS (app f [a])) as

partialS :: ModuleS -> [ModuleS] -> ModuleS
partialS f args | length args > 2  = alternatives $ map g $ init $ split args
                | otherwise        = fail
  where
    g (x, y) = curryS (parenS (f `app` x)) y

app :: ModuleS -> [ModuleS] -> ModuleS
app f as  =  introExprNormalApplication (length as) <*> f <*> sequence as

opS :: String -> Maybe ModuleS -> Maybe ModuleS -> ModuleS
opS n l r = case (l, r) of 
              (Just x, Just y)   -> app (infixApp False False <*> op) [x, y]    <|> 
                                    app (infixApp True  False <*> x  <*> op) [y] <|> 
                                    app (infixApp False True  <*> op <*>  y) [x] <|> 
                                    infixApp True True   <*> x  <*> op <*> y
              (Nothing, Just y)  -> infixApp False True  <*> op <*> y
              (Just x, Nothing)  -> infixApp True  False <*> x  <*> op
              (Nothing, Nothing) -> infixApp False False <*> op         
  where 
    op = introExprVariable <*> introNameOperator n
    infixApp l r = introExprInfixApplication l r 

etaS :: ModuleS -> ModuleS -- f => \x -> f x ;
etaS expr = expr <|> parenS (lambdaS arg (app expr arg))
  where 
    arg = [varS "x"] -- check if x is not elem freevars

{-
-- wil do this strat. when needed
etaFunS :: ModuleS -> ModuleS -> ModuleS
etaFunS name expr =  introFunctionBindings 1 <*> introLHSFun 1 <*> name <*> introPatternVariable <*>
                     introNameIdentifier "x" <*> introRHSExpr 0 <*> introExprNormalApplication 1 <*> expr <*>
                     introExprVariable <*> introNameIdentifier "x"
                 <|> introPatternBinding <*> introPatternVariable <*> name <*> introRHSExpr 0 <*> etaS expr
-}

lambdaS :: [ModuleS] -> ModuleS -> ModuleS
lambdaS args expr =  alternatives $ map f $ split args
  where
    f (xs, ys) = introExprLambda (length xs) <*> sequence xs <*> rec ys
    rec ys = case ys of
               [] -> expr
               ps -> lambdaS ps expr


betaS :: ModuleS -> ModuleS
betaS = undefined

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


compS :: ModuleS -> ModuleS -> ModuleS -- f . g -> \x -> f (g x) 
compS f g  =  opS "." (Just f) (Just g)
          <|> lambdaS [patS "x"] (appS f [parenS (appS g [varS "x"])])


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


--------------------------------------------------------------------------------
-- Help functions
--------------------------------------------------------------------------------
mapSeqS f = sequence . (map f)
repeatS n = sequence . (take n) . repeat

split xs = map (\x -> splitAt x xs) [1..length xs]
