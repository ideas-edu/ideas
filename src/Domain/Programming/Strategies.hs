module Domain.Programming.Strategies
{-   ( fromBinStrategy, getstrat, stringToStrategy, testS
   )-} where

import Common.Context
import Common.Strategy
import Domain.Programming.HeliumRules
import Domain.Programming.Helium
import Domain.Programming.PreludeS


-- test strategy for appS
testS  =  introModule
      <*> introDecls 2
      <*> declFunS [ funS "f" [patS "a", patS "b", patS "c", patS "d"] (varS "a")  [] ]
      <*> declPatS "g" (varS "f" # [intS "1", intS "2", intS "3", intS "4"]) []

-- | fromBin strategy
fromBinStrategy  =  introModule
                <*> introDecls 1
                <*> introPatternBinding 
                <*> introPatternVariable <*> introNameIdentifier "fromBin"
                <*> foldlS consS nilS

consS = etaS $ etaS ( introExprParenthesized <*> 
               compS ( introExprInfixApplication False False <*> introExprVariable <*> introNameOperator "+"
                     )( introExprInfixApplication False True <*> introExprVariable <*> introNameOperator "*" 
                    <*> introExprLiteral <*> introLiteralInt "2"
                     )
             )

nilS = introExprLiteral <*> introLiteralInt "0"
                           

-- | Strategies derived from the abstract syntax of expressions
--   AG: Use multirec (?) to traverse AST to map every language contruct to rule (a->b) in c.
--   Biplate only allows (a->a) in container b 
stringToStrategy :: String -> Strategy (Context Module)
stringToStrategy = getstrat . either (const (error "Compile error")) id . compile

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


-- | help functions
seqStrategy :: GetStrategy a => [a] -> Strategy (Context Module)
seqStrategy = foldr ((<*>) . getstrat) succeed 

-- | test stuff
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
