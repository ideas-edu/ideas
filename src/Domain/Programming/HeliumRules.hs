module Domain.Programming.HeliumRules where

import Common.Context
import Common.Transformation
import Common.Apply
import Domain.Programming.Helium
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad

import Data.Generics.Biplate
import Data.Generics.PlateData
import Data.Data hiding (Fixity)


-- Empty programming AST
introModule :: Rule (Context Module)
introModule = minorRule $ makeSimpleRule "Intro module" (const (return (inContext emptyProg)))

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

introPatternBinding :: Rule (Context Module)
introPatternBinding = toRule "Introduce pattern binding" undefDecls f
  where 
    f = Declaration_PatternBinding noRange undefPattern undefRHS


--------------------------------------------------------------------------------
-- Patterns
--------------------------------------------------------------------------------
introPatternVariable :: String -> Rule (Context Module)
introPatternVariable = toRule "Introduce pattern variable" undefPatterns . f
  where
    f name = Pattern_Variable noRange (Name_Identifier noRange [] name)


--------------------------------------------------------------------------------
-- RightHandSide
--------------------------------------------------------------------------------
introRHSExpr :: Rule (Context Module)
introRHSExpr = toRule "Introduce pattern variable" undefRHSs f
  where
    f = RightHandSide_Expression noRange undefExpr MaybeDeclarations_Nothing


--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

introNormalApplication :: Int -> Rule (Context Module)
introNormalApplication = toRule "Introduce application" undefExprs . f
  where 
    f nargs = Expression_NormalApplication noRange undefExpr $ take nargs $ repeat undefExpr

introInfixApplication :: Rule (Context Module)
introInfixApplication = toRule "Introduce operator" undefExprs e
  where -- need to do something with the operands ... maybe undefExpr...
    e = Expression_InfixApplication noRange MaybeExpression_Nothing undefExpr MaybeExpression_Nothing

-- Variables
introIdentifier :: String -> Rule (Context Module)
introIdentifier = toRule "Intro identifier" undefExprs . f
  where 
    f name = Expression_Variable noRange $ Name_Identifier noRange [] name

introOperator :: String -> Rule (Context Module)
introOperator = toRule "Intro operator" undefExprs . f
  where 
    f name = Expression_Variable noRange $ Name_Operator noRange [] name

-- Literals
introInt :: String -> Rule (Context Module)
introInt = toRule "Intro int" undefExprs . f
  where 
    f i = Expression_Literal noRange $ Literal_Int noRange i

introString :: String -> Rule (Context Module)
introString = toRule "Intro string" undefExprs . f
  where
    f s = Expression_Literal noRange $ Literal_String noRange s


--------------------------------------------------------------------------------
-- Help functions
--------------------------------------------------------------------------------

type Undefs a = Module -> [(a, a -> Module)]

undefs :: (Data.Data.Data a, Eq a) => a -> Undefs a
undefs undef = filter ((== undef) . fst) . contextsBi

undefDecls    = undefs undefDecl
undefRHSs     = undefs undefRHS
undefExprs    = undefs undefExpr
undefPatterns = undefs undefPattern

toRule :: String -> Undefs a -> a -> Rule (Context Module)
toRule s u = makeSimpleRule s . (replaceFirstUndef u)

replaceFirstUndef :: Undefs a -> a -> (Context Module) -> Maybe (Context Module)
replaceFirstUndef u a m = case head (u (fromContext m)) of
                              f -> return $ inContext $ (snd f) $ a
                              _ -> Nothing  

--------------------------------------------------------------------------------
-- Test stuff
--------------------------------------------------------------------------------

sumAST :: Module
sumAST = Module_Module (range (1,1)) MaybeName_Nothing MaybeExports_Nothing 
           (Body_Body (range (1,1)) [] [ sumDecl ])

sumDecl :: Declaration
sumDecl = Declaration_PatternBinding 
            (range (1,1))
            (Pattern_Variable (range (1,1)) (Name_Identifier (range (1,1)) [] "mysum")) 
            (RightHandSide_Expression 
               (range (1,7))
               sumExpr
               MaybeDeclarations_Nothing
            )

sumExpr :: Expression
sumExpr = Expression_NormalApplication 
            (range (1,9))
            (Expression_Variable (range (1,9)) (Name_Identifier (range (1,9)) [] "foldr")) 
            [ Expression_InfixApplication 
                (range (1,15))
                MaybeExpression_Nothing 
                (Expression_Variable (range (1,16)) (Name_Operator (range (1,16)) [] "+")) 
                MaybeExpression_Nothing
            , Expression_Literal 
                (range (1,19))
                (Literal_Int (range (1,19)) "0")
            ]

sumExpr' :: Expression
sumExpr' = Expression_NormalApplication 
            (range (1,9))
            undefExpr
            [ undefExpr, undefExpr ]


-- todo:

{-
getRules :: Module -> [Rule (Context Module)]
getRules expr = 
   case expr of
      Lambda x e -> introLambda x : getRules e
      MatchList b n c -> introMatchList : getRules b ++ getRules n ++ getRules c
      Var x -> introVar x : []
      -- Let x b d -> introLet x : getRules b ++ getRules d
      Apply (Lambda f b) (Fix (Lambda g e)) | f==g -> introLet f : getRules b ++ getRules e
      Fix (Lambda x e) -> getRules (makeLet x e (Var "x"))
      Apply f a -> introApply : getRules f ++ getRules a
      IfThenElse c t e -> introIf : getRules c ++ getRules t ++ getRules e
      _ -> error (show expr)
-}


--------------------------------------------------------------------------------
-- Old stuff
--------------------------------------------------------------------------------

-- use GP (EMGM) for this
liftDecl :: (Declaration -> Maybe Declaration) -> Module -> Maybe Module
liftDecl f (Module_Module mrange mname exps (Body_Body brange imps (d:ds))) = do 
  d' <- f d
  return (Module_Module mrange mname exps (Body_Body brange imps (d':ds)))
liftDecl _ _ = Nothing

-- Needs to be extended with all decl constructors
liftExpression :: (Expression -> Maybe Expression) -> Declaration -> Maybe Declaration
liftExpression f d = case d of
  (Declaration_FunctionBindings rd bs)  -> undefined
  (Declaration_PatternBinding rd p rhs) -> case rhs of
    (RightHandSide_Expression re expr w) -> f expr >>= \ expr' -> return
                                             (Declaration_PatternBinding rd p (RightHandSide_Expression re expr' w))
    (RightHandSide_Guarded rg gexpr w) -> undefined



