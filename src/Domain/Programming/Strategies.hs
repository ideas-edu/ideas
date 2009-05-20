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

-- | fromBin strategy
fromBinStrategy  =  introModule
                <*> introDecls 1
                <*> introPatternBinding 
                <*> introPatternVariable <*> introNameIdentifier "fromBin"
                <*> foldlS (    introExprParenthesized                       -- cons
                            <*> introExprInfixApplication True True
                            <*> introExprInfixApplication False False
                            <*> introExprVariable <*> introNameOperator "+"
                            <*> introExprVariable <*> introNameOperator "."
                            <*> introExprInfixApplication False True
                            <*> introExprVariable <*> introNameOperator "*" 
                            <*> introExprLiteral <*> introLiteralInt "2"
                           )
                           (    introExprLiteral <*> introLiteralInt "0"     -- nil
                           )

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
          <|> introExprLambda 1 <*> introPatternVariable <*> introNameIdentifier "aname" {- not in free vars of f and g -} <*>
              introExprNormalApplication 1 <*> f <*> introExprParenthesized <*> introExprNormalApplication 1 <*>
              g <*> introExprVariable <*> introNameIdentifier "aname"

infixS :: ModuleS
infixS = undefined

{-
foldlMaarTochR f v xs = foldr (\x g -> (\a -> g (f a x))) id xs v 

(Module_Module (1,1) MaybeName_Nothing MaybeExports_Nothing (Body_Body (1,1) [] [Declaration_FunctionBindings (1,1) [FunctionBinding_FunctionBinding (1,1) (LeftHandSide_Function (1,1) foldlMaarTochR [Pattern_Variable (1,16) f,Pattern_Variable (1,18) v,Pattern_Variable (1,20) xs]) (RightHandSide_Expression (1,23) (Expression_NormalApplication (1,25) (Expression_Variable (1,25) foldr) [Expression_Parenthesized (1,31) (Expression_Lambda (1,32) [Pattern_Variable (1,33) x,Pattern_Variable (1,35) g] (Expression_Parenthesized (1,40) (Expression_Lambda (1,41) [Pattern_Variable (1,42) a] (Expression_NormalApplication (1,47) (Expression_Variable (1,47) g) [Expression_Parenthesized (1,49) (Expression_NormalApplication (1,50) (Expression_Variable (1,50) f) [Expression_Variable (1,52) a,Expression_Variable (1,54) x])])))),Expression_Variable (1,59) id,Expression_Variable (1,62) xs,Expression_Variable (1,65) v]) MaybeDeclarations_Nothing)]]))
-}


{-
foldlS :: ExprS -> ExprS -> ExprS
foldlS consS nilS i
    =  appN 2 <*> introVar "foldr" <*> consS i <*> nilS i
   <|> lam <*> appN 3 <*> introVar "foldr" <*> consS (i+1) <*> nilS (i+1) <*> var 0
   <|> rec <*> lam <*> introMatchList <*> var 0 <*> focusTop <*> nilS (i+2) <*>
       lam <*> lam <*> appN 2 <*> focusTop <*> consS (i+4) <*> var 1 <*> 
       app <*> var 3 <*> var 0 
-}

--------------------------------------------------------------------------------
-- | Strategies derived from the abstract syntax of expressions
-- AG: Use multirec (?) to traverse AST to map every language contruct to rule (a->b) in c.
-- Biplate only allows (a->a) in container b 
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
      Declaration_FunctionBindings _ funbs -> introFunctionBindings (length funbs) <*>
                                              seqStrategy funbs -- can be made more flexible with other strategy combinators (like parallel)

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
      Expression_Literal           _ lit            -> introExprLiteral <*> getstrat lit
      Expression_Constructor       _ name           -> introExprConstructor <*> getstrat name
      Expression_Parenthesized     _ expr           -> introExprParenthesized <*> getstrat expr
      Expression_List              _ exprs          -> introExprList (length exprs) <*> 
                                                       seqStrategy exprs
      Expression_Tuple             _ exprs          -> introExprTuple (length exprs) <*> 
                                                       seqStrategy exprs
      Expression_Let               _ decls expr     -> introExprLet (length decls) <*> seqStrategy decls <*>
                                                       getstrat expr
      _                                             -> error $ "No instance for: " ++ show expr

instance GetStrategy LeftHandSide where
  getstrat (LeftHandSide_Function _ name ps) = introLHSFun (length ps) <*> 
                                               getstrat name <*> 
                                               seqStrategy ps

instance GetStrategy RightHandSide where
  getstrat rhs = 
    case rhs of 
      RightHandSide_Expression _ expr   mdecls -> 
          case mdecls of 
            MaybeDeclarations_Just decls -> introRHSExpr (length decls) <*> 
                                            getstrat expr <*>
                                            seqStrategy decls
            _                            -> introRHSExpr 0 <*> getstrat expr
      RightHandSide_Guarded    _ gexprs mdecls -> 
          case mdecls of
            MaybeDeclarations_Just decls -> introRHSGuarded (length gexprs) (length decls) <*>
                                            seqStrategy gexprs <*>
                                            seqStrategy decls
            _                            -> introRHSGuarded (length gexprs) 0 <*>
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
      Pattern_Literal          _ l         -> introPatternLiteral <*> getstrat l
      Pattern_Tuple            _ ps        -> introPatternTuple (length ps) <*> seqStrategy ps
      _                                    -> error $ "No instance for: " ++ show p

instance GetStrategy Literal where
  getstrat lit = 
    case lit of 
      Literal_Int    _ val -> toStrategy $ introLiteralInt val
      Literal_String _ val -> toStrategy $ introLiteralString val

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
            <*> introRHSExpr 0
            <*> introExprNormalApplication 2
            <*> introExprVariable <*> introNameIdentifier "foldr"
            <*> introExprInfixApplication False False
            <*> introExprVariable <*> introNameOperator "+"
            <*> introExprLiteral <*> introLiteralInt "0"

sumStrategy' = stringToStrategy sumString

isortString =  "isort []     = []\n"
            ++ "isort (x:xs) = insert x (isort xs)\n\n"
            ++ "insert x []     = [x]\n"
            ++ "insert x (y:ys) | x <= y     = x : y : ys\n"
            ++ "                | otherwise  = y : insert x ys\n"

isortStrategy' = stringToStrategy isortString


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