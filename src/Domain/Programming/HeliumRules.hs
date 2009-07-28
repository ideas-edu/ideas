module Domain.Programming.HeliumRules where

import Common.Context
import Common.Transformation
import Control.Monad
import Data.Char
import Data.Data hiding (Fixity)
import Data.Generics.Biplate
import Data.Generics.PlateData
import Data.List
import Data.Maybe
import Domain.Programming.Helium
--import Domain.Programming.Prog

--------------------------------------------------------------------------------
-- Module
--------------------------------------------------------------------------------
-- Empty programming AST
introModule :: Rule (Context Module)
introModule = minorRule $ makeSimpleRule "Intro module" (const (return (inContext emptyProg)))


--------------------------------------------------------------------------------
-- Body
--------------------------------------------------------------------------------
introDecls :: Int -> Rule (Context Module)
introDecls = minorRule . toRule "Introduce declarations" undefBodies . f
  where
    f ndecls = Body_Body noRange [] $ take ndecls $ repeat undefDecl


--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------
introPatternBinding :: Rule (Context Module)
introPatternBinding = toRule "Introduce pattern binding" undefDecls f
  where 
    f = Declaration_PatternBinding noRange undefPattern undefRHS

introFunctionBindings :: Int -> Rule (Context Module)
introFunctionBindings nbs = minorRule $ toRule "Introduce function binding" undefDecls f
  where 
    f = Declaration_FunctionBindings noRange $ take nbs $ repeat undefFunBind


--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------
introExprNormalApplication :: Int -> Rule (Context Module)
introExprNormalApplication = toRule "Introduce application" undefExprs . f
  where 
    f nargs = Expression_NormalApplication noRange undefExpr $ take nargs $ repeat undefExpr

introExprInfixApplication :: Bool -> Bool -> Rule (Context Module)
introExprInfixApplication hasLExpr hasRExpr = toRule "Introduce operator" undefExprs f
  where
    f = Expression_InfixApplication noRange 
                                    (if hasLExpr 
                                       then MaybeExpression_Just undefExpr 
                                       else MaybeExpression_Nothing) 
                                    undefExpr 
                                    (if hasRExpr 
                                       then MaybeExpression_Just undefExpr 
                                       else MaybeExpression_Nothing) 

introExprLet :: Int -> Rule (Context Module)
introExprLet ndecls = toRule "Introduce operator" undefExprs f
  where
    f = Expression_Let noRange (take ndecls (repeat undefDecl)) undefExpr

introExprLambda :: Int -> Rule (Context Module)
introExprLambda nps = toRule "Introduce operator" undefExprs f
  where
    f = Expression_Lambda noRange (take nps (repeat undefPattern)) undefExpr

introExprConstructor :: Rule (Context Module)
introExprConstructor = toRule "Intro constructor" undefExprs f
  where 
    f = Expression_Constructor noRange undefName

introExprParenthesized :: Rule (Context Module)
introExprParenthesized = toRule "Intro parentheses" undefExprs f
  where 
    f = Expression_Parenthesized noRange undefExpr

introExprList :: Int -> Rule (Context Module)
introExprList = toRule "Intro expr list" undefExprs . f
  where 
    f nexprs = Expression_List noRange $ take nexprs $ repeat undefExpr

introExprTuple :: Int -> Rule (Context Module)
introExprTuple = toRule "Intro expr tuple" undefExprs . f
  where 
    f nexprs = Expression_Tuple noRange $ take nexprs $ repeat undefExpr

-- Variables
introExprVariable :: Rule (Context Module)
introExprVariable = minorRule $ toRule "Intro variable" undefExprs f
  where 
    f = Expression_Variable noRange undefName

-- Literals
introExprLiteral :: Rule (Context Module)
introExprLiteral = minorRule $ toRule "Intro literal" undefExprs f
  where 
    f = Expression_Literal noRange undefLiteral


--------------------------------------------------------------------------------
-- GuardedExpressions
--------------------------------------------------------------------------------
introGuardedExpr :: Rule (Context Module)
introGuardedExpr = toRule "Introduce pattern variable" undefGuardedExprs f
  where
    f = GuardedExpression_GuardedExpression noRange undefExpr undefExpr


--------------------------------------------------------------------------------
-- LeftHandSide
--------------------------------------------------------------------------------
introLHSFun :: Int -> Rule (Context Module)
introLHSFun = toRule "Introduce function name and patterns" undefLHSs . f
  where
    f nps = LeftHandSide_Function noRange undefName $ take nps $ repeat undefPattern


--------------------------------------------------------------------------------
-- RightHandSide
--------------------------------------------------------------------------------
introRHSExpr :: Int -> Rule (Context Module)
introRHSExpr ndecls = toRule "Introduce pattern variable" undefRHSs f
  where
    f = RightHandSide_Expression noRange 
                                 undefExpr 
                                 (if ndecls > 0
                                    then MaybeDeclarations_Just (take ndecls (repeat undefDecl))
                                    else MaybeDeclarations_Nothing)

introRHSGuarded :: Int -> Int -> Rule (Context Module)
introRHSGuarded ngexprs ndecls = toRule "Introduce pattern variable" undefRHSs f
  where
    f = RightHandSide_Guarded noRange 
                              (take ngexprs (repeat undefGuardedExpr)) 
                              (if ndecls > 0
                                 then MaybeDeclarations_Just (take ndecls (repeat undefDecl))
                                 else MaybeDeclarations_Nothing)



--------------------------------------------------------------------------------
-- Patterns
--------------------------------------------------------------------------------
introPatternVariable :: Rule (Context Module)
introPatternVariable = toRule "Introduce pattern variable" undefPatterns f
  where
    f = Pattern_Variable noRange undefName

introPatternConstructor :: Int -> Rule (Context Module)
introPatternConstructor = toRule "Introduce pattern constructor" undefPatterns . f
  where
    f nps = Pattern_Constructor noRange undefName $ take nps $ repeat undefPattern

introPatternInfixConstructor :: Rule (Context Module)
introPatternInfixConstructor = toRule "Introduce infix pattern constructor" undefPatterns f
  where
    f = Pattern_InfixConstructor noRange undefPattern undefName undefPattern

introPatternParenthesized :: Rule (Context Module)
introPatternParenthesized = toRule "Introduce pattern parentheses" undefPatterns f
  where
    f = Pattern_Parenthesized noRange undefPattern

introPatternLiteral :: Rule (Context Module)
introPatternLiteral = toRule "Introduce literal pattern" undefPatterns f
  where
    f = Pattern_Literal noRange undefLiteral

introPatternTuple :: Int -> Rule (Context Module)
introPatternTuple nps = toRule "Introduce tuple pattern" undefPatterns f
  where
    f = Pattern_Tuple noRange $ take nps $ repeat undefPattern


--------------------------------------------------------------------------------
-- Literals
--------------------------------------------------------------------------------
introLiteralInt :: String -> Rule (Context Module)
introLiteralInt = toRule "Introduce a literal integer" undefLiterals . f
  where
    f = Literal_Int noRange

introLiteralString :: String -> Rule (Context Module)
introLiteralString = toRule "Introduce a literal string" undefLiterals . f
  where
    f = Literal_String noRange

--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------
introNameIdentifier :: String -> Rule (Context Module)
introNameIdentifier = toRule "Introduce identifier name" undefNames . f
  where
    f = Name_Identifier noRange []

introNameOperator :: String -> Rule (Context Module)
introNameOperator = toRule "Introduce operator name" undefNames . f
  where
    f = Name_Operator noRange []

introNameSpecial :: String -> Rule (Context Module)
introNameSpecial = toRule "Introduce special name" undefNames . f
  where
    f = Name_Special noRange []


--------------------------------------------------------------------------------
-- Help functions
--------------------------------------------------------------------------------

type Undefs a = Module -> [(a, a -> Module)]

undefs :: (Data.Data.Data a, Eq a) => a -> Undefs a
undefs undef = filter ((== undef) . fst) . contextsBi

undefDecls        = undefs undefDecl
undefRHSs         = undefs undefRHS
undefLHSs         = undefs undefLHS
undefExprs        = undefs undefExpr
undefPatterns     = undefs undefPattern
undefFunBinds     = undefs undefFunBind
undefGuardedExprs = undefs undefGuardedExpr
undefNames        = undefs undefName
undefBodies       = undefs undefBody
undefLiterals     = undefs undefLiteral

toRule :: String -> Undefs a -> a -> Rule (Context Module)
toRule s u = makeSimpleRule s . (replaceFirstUndef u)

replaceFirstUndef :: Undefs a -> a -> (Context Module) -> Maybe (Context Module)
replaceFirstUndef u a m = let us = u (fromContext m) in
    if not (null us) 
    then
        case head us of
          f -> return $ inContext $ (snd f) $ a
          _ -> Nothing  
    else
        error $ "error undefs" ++ show m



-- Typed holes in a incomplete program
undef = Name_Special noRange [] "undefined"

undefDecl :: Declaration
undefDecl = Declaration_Empty noRange

undefBody :: Body
undefBody = Body_Body noRange [] []

undefExpr :: Expression
undefExpr = Expression_Variable noRange undef

undefGuardedExpr :: GuardedExpression
undefGuardedExpr = GuardedExpression_GuardedExpression noRange undefExpr undefExpr

undefLHS :: LeftHandSide
undefLHS = LeftHandSide_Function noRange undef []

undefRHS :: RightHandSide
undefRHS = RightHandSide_Expression noRange undefExpr MaybeDeclarations_Nothing

undefFunBind :: FunctionBinding
undefFunBind = FunctionBinding_FunctionBinding noRange undefLHS undefRHS

undefPattern :: Pattern
undefPattern = Pattern_Variable noRange undef

undefLiteral :: Literal
undefLiteral = Literal_String noRange "undefined"

undefName :: Name
undefName = undef

-- help functions
range :: (Int, Int) -> Range
range (line, col) = Range_Range (Position_Position "" line col) Position_Unknown

emptyProg =  Module_Module noRange MaybeName_Nothing MaybeExports_Nothing undefBody

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


--------------------------------------------------------------------
-- Derived instances

deriving instance Show Module
deriving instance Show Body
deriving instance Show MaybeName
deriving instance Show MaybeNames
deriving instance Show MaybeExports
deriving instance Show Declaration
deriving instance Show ImportDeclaration
deriving instance Show Export 
deriving instance Show Type
deriving instance Show RightHandSide
deriving instance Show Pattern
deriving instance Show Constructor
deriving instance Show FunctionBinding
deriving instance Show MaybeInt
deriving instance Show Fixity
deriving instance Show MaybeDeclarations
deriving instance Show SimpleType
deriving instance Show ContextItem
deriving instance Show MaybeImportSpecification
deriving instance Show Expression
deriving instance Show RecordPatternBinding
deriving instance Show Literal
deriving instance Show GuardedExpression
deriving instance Show FieldDeclaration
deriving instance Show AnnotatedType
deriving instance Show LeftHandSide
deriving instance Show ImportSpecification
deriving instance Show RecordExpressionBinding
deriving instance Show MaybeExpression
deriving instance Show Statement
deriving instance Show Qualifier
deriving instance Show Alternative
deriving instance Show Import

deriving instance Eq Module
deriving instance Eq Body
deriving instance Eq MaybeName
deriving instance Eq MaybeNames
deriving instance Eq MaybeExports
deriving instance Eq ImportDeclaration
deriving instance Eq Export 
deriving instance Eq Expression
deriving instance Eq Type
deriving instance Eq RecordExpressionBinding
deriving instance Eq Literal
deriving instance Eq Declaration
deriving instance Eq Pattern
deriving instance Eq MaybeExpression
deriving instance Eq Statement
deriving instance Eq Qualifier
deriving instance Eq Alternative
deriving instance Eq RightHandSide
deriving instance Eq Constructor
deriving instance Eq FunctionBinding
deriving instance Eq MaybeInt
deriving instance Eq Fixity
deriving instance Eq MaybeDeclarations
deriving instance Eq SimpleType
deriving instance Eq FieldDeclaration
deriving instance Eq AnnotatedType
deriving instance Eq LeftHandSide
deriving instance Eq ContextItem
deriving instance Eq RecordPatternBinding
deriving instance Eq GuardedExpression
deriving instance Eq MaybeImportSpecification
deriving instance Eq ImportSpecification
deriving instance Eq Import

deriving instance Data Module
deriving instance Data Range
deriving instance Data Position
deriving instance Data Name
deriving instance Data Body
deriving instance Data MaybeName
deriving instance Data MaybeNames
deriving instance Data MaybeExports
deriving instance Data Declaration
deriving instance Data ImportDeclaration
deriving instance Data Export 
deriving instance Data Type
deriving instance Data RightHandSide
deriving instance Data Pattern
deriving instance Data Constructor
deriving instance Data FunctionBinding
deriving instance Data MaybeInt
deriving instance Data Fixity
deriving instance Data MaybeDeclarations
deriving instance Data SimpleType
deriving instance Data ContextItem
deriving instance Data MaybeImportSpecification
deriving instance Data Expression
deriving instance Data RecordPatternBinding
deriving instance Data Literal
deriving instance Data GuardedExpression
deriving instance Data FieldDeclaration
deriving instance Data AnnotatedType
deriving instance Data LeftHandSide
deriving instance Data ImportSpecification
deriving instance Data RecordExpressionBinding
deriving instance Data MaybeExpression
deriving instance Data Statement
deriving instance Data Qualifier
deriving instance Data Alternative
deriving instance Data Import

deriving instance Typeable Module
deriving instance Typeable Range
deriving instance Typeable Position
deriving instance Typeable Name
deriving instance Typeable Body
deriving instance Typeable MaybeName
deriving instance Typeable MaybeNames
deriving instance Typeable MaybeExports
deriving instance Typeable Declaration
deriving instance Typeable ImportDeclaration
deriving instance Typeable Export 
deriving instance Typeable Type
deriving instance Typeable RightHandSide
deriving instance Typeable Pattern
deriving instance Typeable Constructor
deriving instance Typeable FunctionBinding
deriving instance Typeable MaybeInt
deriving instance Typeable Fixity
deriving instance Typeable MaybeDeclarations
deriving instance Typeable SimpleType
deriving instance Typeable ContextItem
deriving instance Typeable MaybeImportSpecification
deriving instance Typeable Expression
deriving instance Typeable RecordPatternBinding
deriving instance Typeable Literal
deriving instance Typeable GuardedExpression
deriving instance Typeable FieldDeclaration
deriving instance Typeable AnnotatedType
deriving instance Typeable LeftHandSide
deriving instance Typeable ImportSpecification
deriving instance Typeable RecordExpressionBinding
deriving instance Typeable MaybeExpression
deriving instance Typeable Statement
deriving instance Typeable Qualifier
deriving instance Typeable Alternative
deriving instance Typeable Import


