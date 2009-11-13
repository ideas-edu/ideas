{-# OPTIONS -XStandaloneDeriving -XDeriveDataTypeable #-}

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
-- Refinement rules for the programming domain.
--
-----------------------------------------------------------------------------

module Domain.Programming.HeliumRules where

import Common.Context
import Common.Transformation
import Common.Utils (safeHead)
import Common.View
import Control.Monad
import Data.Char
import Data.Data hiding (Fixity)
import Data.Generics.Biplate ()
import Data.Generics.PlateData
import Data.List
import Data.Maybe
import Domain.Programming.Helium

--------------------------------------------------------------------------------
-- Module
--------------------------------------------------------------------------------
-- Empty programming AST
introModule :: Rule (Context Module)
introModule = minorRule $ makeSimpleRule "Intro module" $ const $ return $ inContext emptyProg


--------------------------------------------------------------------------------
-- Body
--------------------------------------------------------------------------------
introDecls :: Int -> Rule (Context Module)
introDecls = minorRule . toRule "Introduce declarations" undefBodies . f
  where
    f ndecls = Body_Body noRange [] $ replicate ndecls undefDecl


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
    f = Declaration_FunctionBindings noRange $ replicate nbs undefFunBind


--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------
introExprNormalApplication :: Int -> Rule (Context Module)
introExprNormalApplication = toRule "Introduce application" undefExprs . f
  where 
    f nargs = Expression_NormalApplication noRange undefExpr $ replicate nargs undefExpr

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
    f = Expression_Let noRange (replicate ndecls undefDecl) undefExpr

introExprLambda :: Int -> Rule (Context Module)
introExprLambda nps = toRule "Introduce operator" undefExprs f
  where
    f = Expression_Lambda noRange (replicate nps undefPattern) undefExpr

introExprConstructor :: Rule (Context Module)
introExprConstructor = toRule "Intro constructor" undefExprs f
  where 
    f = Expression_Constructor noRange undefName

introExprParenthesized :: Rule (Context Module)
introExprParenthesized = toRule "Intro parentheses" undefExprs f
  where 
    f = Expression_Parenthesized noRange undefExpr

introExprList :: Int -> Rule (Context Module)
introExprList = ignoreContext . liftRuleIn (transformFirstView) . introExprList' 

{- toRule "Intro expr list" undefExprs . f
  where 
    f nexprs = Expression_List noRange $ replicate nexprs undefExpr
-}
transformFirstView :: View Module (Expression, Expression -> Module)
transformFirstView = makeView f g
 where
   f m = safeHead (undefExprs m)
   g (e, f) = f e

introExprList' :: Int -> Rule Expression
introExprList' n = makeSimpleRule " Intro expr list" $ \e -> do
   guard (e==undefExpr) 
   return $ Expression_List noRange (replicate n undefExpr)

introExprTuple :: Int -> Rule (Context Module)
introExprTuple = toRule "Intro expr tuple" undefExprs . f
  where 
    f nexprs = Expression_Tuple noRange $ replicate nexprs undefExpr

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
    f nps = LeftHandSide_Function noRange undefName $ replicate nps undefPattern


--------------------------------------------------------------------------------
-- RightHandSide
--------------------------------------------------------------------------------
introRHSExpr :: Int -> Rule (Context Module)
introRHSExpr ndecls = toRule "Introduce pattern variable" undefRHSs f
  where
    f = RightHandSide_Expression noRange 
                                 undefExpr 
                                 (if ndecls > 0
                                    then MaybeDeclarations_Just (replicate ndecls undefDecl)
                                    else MaybeDeclarations_Nothing)

introRHSGuarded :: Int -> Int -> Rule (Context Module)
introRHSGuarded ngexprs ndecls = toRule "Introduce pattern variable" undefRHSs f
  where
    f = RightHandSide_Guarded noRange 
                              (replicate ngexprs undefGuardedExpr)
                              (if ndecls > 0
                                 then MaybeDeclarations_Just (replicate ndecls undefDecl)
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
    f nps = Pattern_Constructor noRange undefName $ replicate nps undefPattern

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
    f = Pattern_Tuple noRange $ replicate nps undefPattern

introPatternWildcard :: Rule (Context Module)
introPatternWildcard = toRule "Introduce wildcard pattern" undefPatterns f
  where
    f = Pattern_Wildcard noRange

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

undefs :: (Data a, Eq a) => a -> Undefs a
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
toRule s u = makeSimpleRule s . replaceFirstUndef u

replaceFirstUndef :: Undefs a -> a -> (Context Module) -> Maybe (Context Module)
replaceFirstUndef u a m = do
  f <- safeHead $ u $ fromContext m
  return $ inContext $ snd f a

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

deriving instance Ord Module
deriving instance Ord Body
deriving instance Ord MaybeName
deriving instance Ord MaybeNames
deriving instance Ord MaybeExports
deriving instance Ord Declaration
deriving instance Ord ImportDeclaration
deriving instance Ord Export 
deriving instance Ord Type
deriving instance Ord RightHandSide
deriving instance Ord Pattern
deriving instance Ord Constructor
deriving instance Ord FunctionBinding
deriving instance Ord MaybeInt
deriving instance Ord Fixity
deriving instance Ord MaybeDeclarations
deriving instance Ord SimpleType
deriving instance Ord ContextItem
deriving instance Ord MaybeImportSpecification
deriving instance Ord Expression
deriving instance Ord RecordPatternBinding
deriving instance Ord Literal
deriving instance Ord GuardedExpression
deriving instance Ord FieldDeclaration
deriving instance Ord AnnotatedType
deriving instance Ord LeftHandSide
deriving instance Ord ImportSpecification
deriving instance Ord RecordExpressionBinding
deriving instance Ord MaybeExpression
deriving instance Ord Statement
deriving instance Ord Qualifier
deriving instance Ord Alternative
deriving instance Ord Import
