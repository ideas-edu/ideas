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
--
-----------------------------------------------------------------------------

module Domain.Programming.Strategies
{-   ( fromBinStrategy, stringToStrategy, stringToRules, getRules
   , fromBinFoldlS, fromBinRecurS, fromBinInnerProductS
   )-} where

import Common.Strategy
import Common.Transformation
import Domain.Programming.HeliumRules
import Domain.Programming.Helium
import Domain.Programming.PreludeS
import Prelude hiding (sequence)


-- | fromBin strategy
fromBinStrategy = label "fromBin :: [Int] -> Int" $
  fromBinFoldlS <|> fromBinRecurS <|> fromBinInnerProductS

fromBinFoldlS = label "foldl" $ 
  progS [ declPatS "fromBin" (foldlS consS nilS) [] ]
    where
      consS = exprParenS $ compS (opS "+" Nothing Nothing) (opS "*" Nothing (Just (intS "2")))
      nilS = intS "0"

fromBinRecurS = label "explicit recursion"  $ -- Add tupling model solution! Add an uncurryS to recognise tuples and arguments
  progS [ declFunS [ funS "fromBin" [patConS "[]"] (intS "0") []
                   , funS "fromBin" [patInfixConS (patS "x") ":" (patS "xs")] 
                          (opS "+" (Just (opS "*" (Just (varS "x"))
                                                 (Just (opS "^" (Just (intS "2")) 
                                                                (Just (varS "length" # [varS "xs"]))))))
                                  (Just (varS "fromBin" # [varS "xs"])))
                          []
                   ]
        ]

{-
fromBin x = fromBin' x (length x - 1)
  where fromBin' []     _ = 0
        fromBin' (y:ys) z = y * 2^z + fromBin' ys (z-1)
-}
fromBinTuplingS = label "Tupling"  $ -- Add an uncurryS to recognise tuples and arguments
  progS [ declFunS [ funS "fromBin" [patS "x"] 
    (varS "fromBin'" # [varS "x", opS "-" (Just (varS "length" # [varS "x"])) (Just (intS "1"))]) 
      [  declFunS [ funS "fromBin'" [patConS "[]", patWildcardS] (intS "0") []
                  , funS "fromBin'" [patInfixConS (patS "y") ":" (patS "ys"), patS "z"] 
                          (opS "+" (Just (opS "*" (Just (varS "y"))
                                                  (Just (opS "^" (Just (intS "2")) 
                                                                 (Just (varS "z"))))))
                                   (Just (varS "fromBin'" # [varS "ys", opS "-" (Just (varS "z")) (Just (intS "1"))])))
                          []
                  ]
      ]]]


            

-- fromBin = sum . zipWith (*) (iterate (*2) 1) . reverse
fromBinInnerProductS = label "Inner product" $
  progS [declPatS "fromBin" 
           (compS sumS (compS (zipWithS # [ opS "*" Nothing Nothing
                                          , powersOfTwoS
                                          ])
                               reverseS
                       )
           ) []
        ]
  where
    powersOfTwoS  =  iterateS # [opS "*" Nothing (Just (intS "2")), intS "1"]
{-                 <|> ([base^x | x <- [0..] ])
                 <|> (map (2^) [0..(length l)-1])
                 <|> (lambdaS [varS "xs"] 
                       (varS "scanr" # [ opS "*" Nothing Nothing
                                       , intS "1"
                                       , varS "take" # [ opS "-" (Just (varS "length" # [varS "xs"]))
                                                                 (Just (intS "1"))                
                                                       , varS "repeat" # [intS "2"]
                                                       ]
                                       ]))
-}

{-
  Solution ("fromBin = fromBaseInt 2\n"
         ++ "fromBaseInt base xs = sum $ zipWith (*) bMachten xs\n"
         ++ "  where bMachten = scanr (*) 1 $ take (length xs - 1) $ repeat base\n")
            "bspaans"
            fromBinInnerProductS
            "Good solution"
-}


-- with biplate I don't have a function (a -> b) -> c -> c or more precise (a -> b) -> c a -> c b
--   
-- first traversal of ast, converting 
--type 

-- | Strategies derived from the abstract syntax of expressions
stringToStrategy :: String -> Strategy Module
stringToStrategy = sequence . stringToRules

stringToRules :: String -> [Rule Module]
stringToRules = getRules . either (const (error "Compile error")) id . compile

class GetRules a where
  getRules :: a -> [Rule Module]

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
      _                                    -> error $ "No instance for: " ++ show d

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
  getRules lhs = error $ "No instance for: " ++ show lhs

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
      _                    -> error $ "No instance for: " ++ show lit

instance GetRules Name where
  getRules name = 
    case name of 
      Name_Identifier _ _ name -> [introNameIdentifier name]
      Name_Operator   _ _ name -> [introNameOperator name]
      Name_Special    _ _ name -> [introNameSpecial name]


