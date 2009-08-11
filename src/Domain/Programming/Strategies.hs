module Domain.Programming.Strategies
{-   ( fromBinStrategy, getstrat, stringToStrategy, testS
   )-} where

import Common.Context
import Common.Strategy
import Common.Transformation
import Domain.Programming.HeliumRules
import Domain.Programming.Helium
import Domain.Programming.PreludeS
import Prelude hiding (sequence)

-- test strategy for appS
testS  =  modS [ declFunS [ funS "f" [patS "a", patS "b", patS "c", patS "d"] (varS "a")  [] ]
               , declPatS "g" (varS "f" # [intS "1", intS "2", intS "3", intS "4"]) [] ]

testFunS = modS [ declPatS "f" consS [] ]

-- | fromBin strategy
fromBinStrategy = modS [ introPatternBinding <*> introPatternVariable <*> 
                         introNameIdentifier "fromBin" <*> foldlS consS nilS ]

consS = compS (opS "+" Nothing Nothing) (opS "*" Nothing (Just (intS "2")))

nilS = introExprLiteral <*> introLiteralInt "0"
                           

-- | Strategies derived from the abstract syntax of expressions
--   AG: Use multirec (?) to traverse AST to map every language contruct to rule (a->b) in c.
--   Biplate only allows (a->a) in container b 
stringToStrategy :: String -> Strategy (Context Module)
stringToStrategy = sequence . stringToRules

stringToRules :: String -> [Rule (Context Module)]
stringToRules = getRules . either (const (error "Compile error")) id . compile

class GetRules a where
  getRules :: a -> [Rule (Context Module)]

instance GetRules Module where
  getRules (Module_Module _ _ _ body) = introModule : getRules body

instance GetRules Body where
  getRules (Body_Body _ _ decls) = introDecls (length decls) : concatMap getRules decls

instance GetRules MaybeDeclarations where
  getRules mdecls = 
    case mdecls of
      MaybeDeclarations_Nothing    -> []
      MaybeDeclarations_Just decls -> concatMap getRules decls

instance GetRules Declaration where
  getRules d = 
    case d of 
      Declaration_PatternBinding _ pattern rhs -> introPatternBinding : 
                                                  getRules pattern ++
                                                  getRules rhs
      Declaration_FunctionBindings _ funbs -> introFunctionBindings (length funbs) :
                                              concatMap getRules funbs

instance GetRules MaybeExpression where
  getRules mexpr = 
    case mexpr of
      MaybeExpression_Nothing   -> []
      MaybeExpression_Just expr -> getRules expr

instance GetRules Expression where
  getRules expr = 
    case expr of 
      Expression_NormalApplication _ fun args       -> introExprNormalApplication (length args) :
                                                       getRules fun ++ concatMap getRules args
      Expression_Variable          _ name           -> introExprVariable : getRules name
      Expression_InfixApplication  _ lexpr op rexpr -> introExprInfixApplication (lexpr /= MaybeExpression_Nothing) 
                                                                                 (rexpr /= MaybeExpression_Nothing) :
                                                       getRules lexpr ++ getRules op ++
                                                       getRules rexpr
      Expression_Literal           _ lit            -> introExprLiteral : getRules lit
      Expression_Constructor       _ name           -> introExprConstructor : getRules name
      Expression_Parenthesized     _ expr           -> introExprParenthesized : getRules expr
      Expression_List              _ exprs          -> introExprList (length exprs) : 
                                                       concatMap getRules exprs
      Expression_Tuple             _ exprs          -> introExprTuple (length exprs) :
                                                       concatMap getRules exprs
      Expression_Let               _ decls expr     -> introExprLet (length decls) : concatMap getRules decls ++
                                                       getRules expr
      Expression_Lambda            _ ps expr        -> introExprLambda (length ps) : concatMap getRules ps ++ 
                                                       getRules expr
      _                                             -> error $ "No instance for: " ++ show expr

instance GetRules LeftHandSide where
  getRules (LeftHandSide_Function _ name ps) = introLHSFun (length ps) : getRules name ++
                                               concatMap getRules ps

instance GetRules RightHandSide where
  getRules rhs = 
    case rhs of 
      RightHandSide_Expression _ expr   mdecls -> 
          case mdecls of 
            MaybeDeclarations_Just decls -> introRHSExpr (length decls) :
                                            getRules expr ++
                                            concatMap getRules decls
            _                            -> introRHSExpr 0 : getRules expr
      RightHandSide_Guarded    _ gexprs mdecls -> 
          case mdecls of
            MaybeDeclarations_Just decls -> introRHSGuarded (length gexprs) (length decls) :
                                            concatMap getRules gexprs ++
                                            concatMap getRules decls
            _                            -> introRHSGuarded (length gexprs) 0 :
                                            concatMap getRules gexprs

instance GetRules GuardedExpression where
  getRules (GuardedExpression_GuardedExpression _ guard expr) = introGuardedExpr : 
                                                                getRules guard ++ 
                                                                getRules expr

instance GetRules FunctionBinding where
  getRules (FunctionBinding_FunctionBinding _ lhs rhs) = getRules lhs ++
                                                         getRules rhs

instance GetRules Pattern where
  getRules p = 
    case p of
      Pattern_Variable         _ name     -> introPatternVariable : getRules name
      Pattern_Constructor      _ name ps  -> introPatternConstructor (length ps) :
                                             getRules name ++
                                             concatMap getRules ps
      Pattern_InfixConstructor _ lp op rp -> introPatternInfixConstructor :
                                             getRules op ++ getRules lp ++ getRules rp
      Pattern_Parenthesized    _ p         -> introPatternParenthesized : getRules p
      Pattern_Literal          _ l         -> introPatternLiteral : getRules l
      Pattern_Tuple            _ ps        -> introPatternTuple (length ps) : concatMap getRules ps
      _                                    -> error $ "No instance for: " ++ show p

instance GetRules Literal where
  getRules lit = 
    case lit of 
      Literal_Int    _ val -> [introLiteralInt val]
      Literal_String _ val -> [introLiteralString val]

instance GetRules Name where
  getRules name = 
    case name of 
      Name_Identifier _ _ name -> [introNameIdentifier name]
      Name_Operator   _ _ name -> [introNameOperator name]
      Name_Special    _ _ name -> [introNameSpecial name]


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
