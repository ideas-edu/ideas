{-# OPTIONS -XStandaloneDeriving -XDeriveDataTypeable #-}

---------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
---------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  unknown
--
-- Refinement rules for the programming domain.
--
---------------------------------------------------------------------------

module Domain.Programming.HeliumRules where

import Common.Transformation hiding (liftRule)
import Common.Utils (safeHead)
import Common.View
import Control.Monad
import Data.Char
import Data.Data hiding (Fixity)
import Data.Generics.Biplate ()
import Data.Generics.PlateData
import Data.List
import Domain.Programming.Helium

------------------------------------------------------------------------------
-- | Typed holes
------------------------------------------------------------------------------
class Undefined a where
  undef  :: a
instance Undefined Declaration where
  undef  = Declaration_Empty noRange
instance Undefined RightHandSide where
  undef = RightHandSide_Expression noRange undef MaybeDeclarations_Nothing
instance Undefined LeftHandSide where
  undef = LeftHandSide_Function noRange undef []
instance Undefined Expression where
  undef = Expression_Variable noRange undef
instance Undefined Pattern where
  undef = Pattern_Variable noRange undef
instance Undefined FunctionBinding where
  undef = FunctionBinding_FunctionBinding noRange undef undef
instance Undefined GuardedExpression where
  undef = GuardedExpression_GuardedExpression noRange undef undef
instance Undefined Name where
  undef = Name_Special noRange [] "undefined"
instance Undefined Body where
  undef = Body_Body noRange [] []
instance Undefined Literal where
  undef = Literal_String noRange "undefined"
instance Undefined Module where
  undef = Module_Module noRange MaybeName_Nothing MaybeExports_Nothing undef
instance Undefined MaybeExpression where
  undef = MaybeExpression_Nothing
instance Undefined MaybeDeclarations where
  undef = MaybeDeclarations_Nothing

undefs :: (Data a, Data b, Eq a, Undefined a) => b -> [(a, a -> b)]
undefs = filter ((== undef) . fst) . contextsBi

liftRule :: (Data a, Data b, Eq a, Undefined a) => Rule a -> Rule b
liftRule = liftRuleIn $ makeView (safeHead . undefs) (\(e, f) -> f e)

-- toRule zou met een contexts moeten kunnen, er worden undefs gezocht
-- in een term van hetzelfde type
toRule :: (Data a, Undefined a, Eq a) => String -> a -> Rule a
toRule desc a = makeSimpleRule desc $ \e -> do
  (_, replaceUndef) <- safeHead $ undefs e
  return $ replaceUndef a
{-
  \e -> do
  guard (e == undef) -- dit kan niet goed gaan, er moet worden gezoch naar
  return a           -- een undefined.
-}
  

------------------------------------------------------------------------------
-- Module
------------------------------------------------------------------------------
-- Empty programming AST
introModule :: Rule Module
introModule = minorRule $ makeSimpleRule "Intro module" $ 
  const $ return undef

------------------------------------------------------------------------------
-- Body
------------------------------------------------------------------------------
introBody :: Int -> Rule Body
introBody ndecls = minorRule $ toRule "Introduce declarations" $
  Body_Body noRange [] $ replicate ndecls undef

------------------------------------------------------------------------------
-- Declarations
------------------------------------------------------------------------------
introDeclPatBinding :: Rule Declaration
introDeclPatBinding = minorRule $ toRule "Introduce pattern binding" $
  Declaration_PatternBinding noRange undef undef

introDeclFunBindings :: Int -> Rule Declaration
introDeclFunBindings nbs = minorRule $ toRule "Introduce function bindings" $ 
  Declaration_FunctionBindings noRange $ replicate nbs undef

introMaybeDecls :: Int -> Rule MaybeDeclarations
introMaybeDecls ndecls = minorRule $ toRule "Introduce decl" $ 
  if ndecls > 0 then MaybeDeclarations_Just (replicate ndecls undef)
                else MaybeDeclarations_Nothing

------------------------------------------------------------------------------
-- Expressions
------------------------------------------------------------------------------
introExprNormalApplication :: Int -> Rule Expression
introExprNormalApplication nargs = toRule "Introduce application" $
  Expression_NormalApplication noRange undef $ replicate nargs undef

introExprInfixApplication :: Bool -> Bool -> Rule Expression
introExprInfixApplication hasLExpr hasRExpr = toRule "Introduce operator" $ 
  Expression_InfixApplication noRange 
    (if hasLExpr then MaybeExpression_Just undef else MaybeExpression_Nothing) 
    undef
    (if hasRExpr then MaybeExpression_Just undef else MaybeExpression_Nothing) 

introExprLet :: Int -> Rule Expression
introExprLet ndecls = toRule "Introduce operator" $
  Expression_Let noRange (replicate ndecls undef) undef

introExprLambda :: Int -> Rule Expression
introExprLambda nps = toRule "Introduce operator" $ 
  Expression_Lambda noRange (replicate nps undef) undef

introExprConstructor :: Rule Expression
introExprConstructor = toRule "Intro constructor" $
  Expression_Constructor noRange undef

introExprParenthesized :: Rule Expression
introExprParenthesized = toRule "Intro parentheses" $ 
  Expression_Parenthesized noRange undef

introExprList :: Int -> Rule Expression
introExprList n = toRule "Intro expr list" $ 
  Expression_List noRange $ replicate n undef

introExprTuple :: Int -> Rule Expression
introExprTuple nexprs = toRule "Intro expr tuple" $
  Expression_Tuple noRange $ replicate nexprs undef

-- Variables
introExprVariable :: Rule Expression
introExprVariable = minorRule $ toRule "Intro variable" $
  Expression_Variable noRange undef

-- Literals
introExprLiteral :: Rule Expression
introExprLiteral = minorRule $ toRule "Intro literal" $ 
  Expression_Literal noRange undef

-- MaybeExpressions
introMaybeExpr :: Bool -> Rule MaybeExpression
introMaybeExpr p = minorRule $ toRule "Intro maybeExpression" $ 
  if p then MaybeExpression_Just undef else MaybeExpression_Nothing

------------------------------------------------------------------------------
-- GuardedExpressions
------------------------------------------------------------------------------
introGuardedExpr :: Rule GuardedExpression
introGuardedExpr = toRule "Introduce pattern variable" $ 
  GuardedExpression_GuardedExpression noRange undef undef

-----------------------------------------------------------------------------
-- FunctionBinding
------------------------------------------------------------------------------
introFunBinding :: Rule FunctionBinding
introFunBinding = toRule "Introduce function binding" $ 
  FunctionBinding_FunctionBinding noRange undef undef

------------------------------------------------------------------------------
-- LeftHandSide
------------------------------------------------------------------------------
introLHSFun :: Int -> Rule LeftHandSide
introLHSFun nps = toRule "Introduce function name and patterns" $
  LeftHandSide_Function noRange undef $ replicate nps undef

------------------------------------------------------------------------------
-- RightHandSide
------------------------------------------------------------------------------
introRHSExpr :: Int -> Rule RightHandSide
introRHSExpr ndecls = toRule "Introduce pattern variable" $
  RightHandSide_Expression noRange undef
    (if ndecls > 0 then MaybeDeclarations_Just (replicate ndecls undef)
                   else MaybeDeclarations_Nothing)

introRHSGuarded :: Int -> Int -> Rule RightHandSide
introRHSGuarded ngexprs ndecls = toRule "Introduce pattern variable" $
  RightHandSide_Guarded noRange (replicate ngexprs undef)
    (if ndecls > 0 then MaybeDeclarations_Just (replicate ndecls undef)
                   else MaybeDeclarations_Nothing)

------------------------------------------------------------------------------
-- Patterns
------------------------------------------------------------------------------
introPatternVariable :: Rule Pattern
introPatternVariable = minorRule $ toRule "Introduce pattern variable" $ 
  Pattern_Variable noRange undef

introPatternConstructor :: Int -> Rule Pattern
introPatternConstructor nps = toRule "Introduce pattern constructor" $
  Pattern_Constructor noRange undef $ replicate nps undef

introPatternInfixConstructor :: Rule Pattern
introPatternInfixConstructor = toRule "Introduce infix pattern constructor" $
  Pattern_InfixConstructor noRange undef undef undef

introPatternParenthesized :: Rule Pattern
introPatternParenthesized = toRule "Introduce pattern parentheses" $
  Pattern_Parenthesized noRange undef

introPatternLiteral :: Rule Pattern
introPatternLiteral = toRule "Introduce literal pattern" $
  Pattern_Literal noRange undef

introPatternTuple :: Int -> Rule Pattern
introPatternTuple nps = toRule "Introduce tuple pattern" $
  Pattern_Tuple noRange $ replicate nps undef

introPatternWildcard :: Rule Pattern
introPatternWildcard = toRule "Introduce wildcard pattern" $
  Pattern_Wildcard noRange

------------------------------------------------------------------------------
-- Literals
------------------------------------------------------------------------------
introLiteralInt :: String -> Rule Literal
introLiteralInt = toRule "Introduce a literal integer" . 
  Literal_Int noRange

introLiteralString :: String -> Rule Literal
introLiteralString = toRule "Introduce a literal string" . 
  Literal_String noRange

------------------------------------------------------------------------------
-- Names
------------------------------------------------------------------------------
introNameIdentifier :: String -> Rule Name
introNameIdentifier = toRule "Introduce identifier name" .
  Name_Identifier noRange []

introNameOperator :: String -> Rule Name
introNameOperator = toRule "Introduce operator name" .
  Name_Operator noRange []

introNameSpecial :: String -> Rule Name
introNameSpecial = toRule "Introduce special name" .
  Name_Special noRange []


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
