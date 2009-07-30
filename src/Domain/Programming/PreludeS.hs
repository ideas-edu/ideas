module Domain.Programming.PreludeS
{-   ( ModuleS, foldlS, letS, compS, etaS
   )-} where

import Common.Context hiding (Var)
import Common.Strategy hiding (repeat)
import Domain.Programming.HeliumRules
import Domain.Programming.Helium
import Data.Generics.Biplate
import Data.Generics.PlateData
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


s = modS [ declPatS "f" (parenS $ compS (opS "+" Nothing Nothing) (opS "*" Nothing (Just (intS "2")))) [] ]

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
appS f args = parenS $ app f args
-- appS f args = fix (\ t -> curryS f args <|> infixS f args <|> etaS t)

infixS f args = case args of 
                  x:y:z:zs -> app (parenS (infixApp f x y)) (z:zs)
                  x:y:[]   -> infixApp f x y
                  _        -> fail
  where
    infixApp f l r = introExprInfixApplication True True <*> l <*> f <*> r

{-
curryS :: ModuleS -> [ModuleS] -> ModuleS
curryS f args = case args of 
                  []   -> fail
                  a:as -> app f (a:as) <|> partialS f args 
                                       <|> curryS (parenS (app f [a])) as
-}

curryS :: ModuleS -> [ModuleS] -> ModuleS
curryS f args = fix (\t -> case args of 
                             []   -> fail
                             a:as -> app f (a:as) <|> curryS (parenS (app f [a])) as)

funS' :: Int -> ModuleS -> [ModuleS] -> ModuleS
funS' kind f args  =  appS f args
                 <|> if partargs > 0
                     then lambdaS (take partargs [varS ('x' : show i) | i <- [1..]]) (appS f args)
                     else fail
  where partargs = kind - (length args)

partialS :: ModuleS -> [ModuleS] -> ModuleS
partialS f args | length args > 2  = alternatives $ map g $ init $ split args
                | otherwise        = fail
  where
    g (x, y) = curryS (parenS (f `app` x)) y

app :: ModuleS -> [ModuleS] -> ModuleS
app f as  =  introExprNormalApplication (length as) <*> f <*> sequence as

opS :: String -> Maybe ModuleS -> Maybe ModuleS -> ModuleS
opS n l r = case (l, r) of 
              (Just x, Just y)   -> app (prefix) [x, y] <|> 
                                    app (pleft x) [y] <|> 
                                    app (pright y) [x] <|> 
                                    infixApp True True <*> x  <*> op <*> y
              (Nothing, Just y)  -> pright y <|> parenS (lambdaS [patS "x"] (opS n (Just (varS "x")) r))
              (Just x, Nothing)  -> pleft x  <|> parenS (lambdaS [patS "x"] (opS n l (Just (varS "x"))))
              (Nothing, Nothing) -> prefix   <|> parenS (lambdaS [patS "x", patS "y"] (opS n (Just (varS "x")) (Just (varS "y"))))
  where 
    op = introExprVariable <*> introNameOperator n
    infixApp l r = introExprInfixApplication l r
    pleft x = infixApp True  False <*> x  <*> op
    prefix = infixApp False False <*> op
    pright y = infixApp False True  <*> op <*> y

etaS :: ModuleS -> ModuleS -- f => \x -> f x ;
etaS expr = fix (\t -> expr <|> lambdaS [patS "x"] (app (parenS t) [varS "x"])) -- check if x not elem of freevars!
--etaS expr = fix (\ t -> expr <|> lambdaS [patS "x"] (appS t [varS "x"])) -- check if x not elem of freevars!



{-
-- wil do this strat. when needed
etaFunS :: ModuleS -> ModuleS -> ModuleS
etaFunS name expr =  introFunctionBindings 1 <*> introLHSFun 1 <*> name <*> introPatternVariable <*>
                     introNameIdentifier "x" <*> introRHSExpr 0 <*> introExprNormalApplication 1 <*> expr <*>
                     introExprVariable <*> introNameIdentifier "x"
                 <|> introPatternBinding <*> introPatternVariable <*> name <*> introRHSExpr 0 <*> etaS expr
-}

lambdaS :: [ModuleS] -> ModuleS -> ModuleS
lambdaS args expr = alternatives $ map f $ split args
  where
    f (xs, ys) = introExprLambda (length xs) <*> sequence xs <*> rec ys
    rec ys = case ys of
               [] -> expr
               ps -> lambdaS ps expr

lambda :: ModuleS -> ModuleS -> ModuleS
lambda arg expr = introExprLambda 1 <*> arg <*> expr

betaReduction :: Context Module -> Context Module
betaReduction = inContext . transformBi red . fromContext
  where
    red x = case x of 
              (Expression_NormalApplication _ 
                (Expression_Parenthesized _ 
                  (Expression_Lambda _ ps expr)) as) -> substAllArgs ps as expr 
              _                                      -> x 
    substAllArgs xs ys expr = foldr (\(x,y) -> substArg x y) expr (zip xs ys)
    substArg x y expr = case x of 
      (Pattern_Variable _ x) -> transformBi (subst x y) expr
      _                      -> expr
    subst x y e = case e of
                    (Expression_Variable _ n) -> if n == x then y else e
                    _ -> e



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
parenS expr = expr <|> introExprParenthesized <*> expr

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
