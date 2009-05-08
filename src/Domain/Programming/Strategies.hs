module Domain.Programming.Strategies where

import Prelude hiding (sequence)
import Common.Context
import Common.Strategy
import Common.Uniplate
import Common.Exercise
import Common.Transformation
import Common.Apply
--import Common.Parsing (SyntaxError(..))
import Domain.Programming.Expr
import Domain.Programming.Rules
import Domain.Programming.HeliumRules
import Domain.Programming.Helium
import Domain.Programming.Parser
import Domain.Programming.Prelude (isortE2,insertE,insertNilE,insertConsE)
import Domain.Programming.Eval (eval, mylist)
import Data.Maybe
import Data.Char


-- AG: Use multirec (?) to traverse AST to map every language contruct to rule (a->b) in c.
-- Biplate only allows (a->a) in container b 

--------------------------------------------------------------------------------
-- | Strategies derived from the abstract syntax of expressions
class GetStrategy a where
  getstrat :: a -> Strategy (Context Module)

instance GetStrategy Module where
  getstrat (Module_Module _ _ _ body) = introModule <*> getstrat body

instance GetStrategy Body where
  getstrat (Body_Body _ _ decls) = introDecls (length decls) <*> seqStrategy decls

instance GetStrategy MaybeDeclarations where
  getstrat mdecls = 
    case mdecls of
      MaybeDeclarations_Nothing    -> succeed -- ! could result to unexpected behaviour, when using choice <|> !
      MaybeDeclarations_Just decls -> seqStrategy decls

instance GetStrategy Declaration where
  getstrat d = 
    case d of 
      Declaration_PatternBinding _ pattern rhs -> introPatternBinding <*> 
                                                  getstrat pattern <*>
                                                  getstrat rhs
      Declaration_FunctionBindings _ funbs -> introFunctionBindings (length funbs) 
                                          <*> seqStrategy funbs -- can be made more flexible with other strategy combinators (like parallel)

instance GetStrategy MaybeExpression where
  getstrat mexpr = 
    case mexpr of
      MaybeExpression_Nothing   -> succeed -- ! could result to unexpected behaviour, when using choice <|> !
      MaybeExpression_Just expr -> getstrat expr

instance GetStrategy Expression where
  getstrat expr = 
    case expr of 
      Expression_NormalApplication _ fun args       -> introExprNormalApplication (length args) <*>
                                                       getstrat fun <*> seqStrategy args
      Expression_Variable          _ name           -> introExprVariable <*> getstrat name
      Expression_InfixApplication  _ lexpr op rexpr -> introExprInfixApplication (lexpr /= MaybeExpression_Nothing) 
                                                                                 (rexpr /= MaybeExpression_Nothing) <*> 
                                                       getstrat lexpr <*> getstrat op <*>
                                                       getstrat rexpr
      Expression_Literal           _ lit            -> toStrategy $ case lit of
                                                                      Literal_Int _ val -> introInt val
      Expression_Constructor       _ name           -> introExprConstructor <*> getstrat name
      Expression_Parenthesized     _ expr           -> introExprParenthesized <*> getstrat expr
      Expression_List              _ exprs          -> introExprList (length exprs) <*> 
                                                       seqStrategy exprs

instance GetStrategy LeftHandSide where
  getstrat (LeftHandSide_Function _ name ps) = introLHSFun (length ps) <*> 
                                               getstrat name <*> 
                                               seqStrategy ps

instance GetStrategy RightHandSide where
  getstrat rhs = 
    case rhs of 
      RightHandSide_Expression _ expr   _ -> introRHSExpr <*> getstrat expr
      RightHandSide_Guarded    _ gexprs _ -> introRHSGuarded (length gexprs) <*>
                                             seqStrategy gexprs

instance GetStrategy GuardedExpression where
  getstrat (GuardedExpression_GuardedExpression _ guard expr) = introGuardedExpr <*> 
                                                                getstrat guard <*> 
                                                                getstrat expr

instance GetStrategy FunctionBinding where
  getstrat (FunctionBinding_FunctionBinding _ lhs rhs) = getstrat lhs <*>
                                                         getstrat rhs

instance GetStrategy Pattern where
  getstrat p = 
    case p of
      Pattern_Variable         _ name     -> introPatternVariable <*> getstrat name
      Pattern_Constructor      _ name ps  -> introPatternConstructor (length ps) <*>
                                             getstrat name <*>
                                             seqStrategy ps
      Pattern_InfixConstructor _ lp op rp -> introPatternInfixConstructor <*>
                                             getstrat op <*> getstrat lp <*> getstrat rp
      Pattern_Parenthesized    _ p         -> introPatternParenthesized <*> getstrat p

instance GetStrategy Name where
  getstrat name = 
    case name of 
      Name_Identifier _ _ name -> toStrategy $ introNameIdentifier name
      Name_Operator   _ _ name -> toStrategy $ introNameOperator name
      Name_Special    _ _ name -> toStrategy $ introNameSpecial name


-- help functions
stringToStrategy :: String -> Strategy (Context Module)
stringToStrategy = getstrat . either (const (error "Compile error")) id . compile

seqStrategy :: GetStrategy a => [a] -> Strategy (Context Module)
seqStrategy = foldr ((<*>) . getstrat) succeed 

-- test stuff
sumString = "mysum = foldr (+) 0"
sumStrategy  =  introModule
            <*> introDecls 1
            <*> introPatternBinding 
            <*> introPatternVariable <*> introNameIdentifier "mysum"
            <*> introRHSExpr 
            <*> introExprNormalApplication 2
            <*> introExprVariable <*> introNameIdentifier "mysum"
            <*> introExprInfixApplication False False
            <*> introExprVariable <*> introNameOperator "+"
            <*> introInt "0"

sumStrategy' = stringToStrategy sumString

isortString =  "isort []     = []\n"
            ++ "isort (x:xs) = insert x (isort xs)\n\n"
            ++ "insert x []     = [x]\n"
            ++ "insert x (y:ys) | x <= y     = x : y : ys\n"
            ++ "                | otherwise  = y : insert x ys\n"

isortStrategy' = stringToStrategy isortString




-- Old stuff
getStrategy :: Expr -> Strategy (Context Expr)
getStrategy expr = 
   case expr of
      Lambda x e -> introLambda x <*> getStrategy e
      MatchList b n c -> introMatchList <*> getStrategy b <*> getStrategy n <*> getStrategy c
      Var x -> toStrategy (introVar x)
      --Let x b d -> introLet x <*> getStrategy b <*> getStrategy d
      Apply (Lambda f body) (Fix (Lambda g e)) | f==g ->
         introLet f <*> getStrategy e <*> getStrategy body
      Fix (Lambda f e) -> getStrategy (makeLet f e (Var f))
      Apply f a -> introApply <*> getStrategy f <*> getStrategy a
      IfThenElse c t e -> introIf <*> getStrategy c <*> getStrategy t <*> getStrategy e

{- The abstract strategy has a slightly more abstract representation
of list pattern matching. The following function is partial, and
only works on pattern matching over variables. -}
getAbstractStrategy :: Expr -> Strategy (Context Expr)
getAbstractStrategy expr = 
   case expr of
      Lambda x e -> introLambda x <*> getAbstractStrategy e
      MatchList b n c -> listPatternMatching b (getLambda c) (getLambda (dropLambda c)) <*> getAbstractStrategy n <*> getAbstractStrategy (dropLambda (dropLambda c))
      Var x -> toStrategy (introVar x)
      Apply (Lambda f body) (Fix (Lambda g e)) | f==g -> 
         introLet f <*> getAbstractStrategy e <*> getAbstractStrategy body
      Fix (Lambda f e) -> getAbstractStrategy (makeLet f e (Var f))
      Apply f a -> introApply <*> getAbstractStrategy f <*> getAbstractStrategy a
      IfThenElse c t e -> introIf <*> getAbstractStrategy c <*> getAbstractStrategy t <*> getAbstractStrategy e
      
getLambda (Lambda x e)   = x
getLambda expr           = error "Not a lambda"

dropLambda (Lambda x e)  = e
dropLambda expr          = error "Not a lambda"
 
-- the insertion sort strategy with a fold (Bastiaan)

isortStrategy :: Strategy (Context Expr)
isortStrategy = getStrategy isortE2

isortAbstractStrategy :: Strategy (Context Expr)
isortAbstractStrategy = getAbstractStrategy isortE2

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