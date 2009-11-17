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
-- | Typed holes
--------------------------------------------------------------------------------
undefs :: (Data a, Eq a, Undefined a) => Module -> [(a, a -> Module)]
undefs = filter ((== undef) . fst) . contextsBi

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

liftRuleInModule :: (Data a, Eq a, Undefined a) => Rule a -> Rule Module
liftRuleInModule = liftRuleIn $ makeView (safeHead . undefs) (\(e, f) -> f e)

toRule :: String -> (Module -> [(a, a -> Module)]) -> a -> Rule (Context Module)
toRule s u = makeSimpleRule s . replaceFirstUndef u

replaceFirstUndef :: (Module -> [(a, a -> Module)]) -> a -> (Context Module) -> Maybe (Context Module)
replaceFirstUndef u a m = do
  f <- safeHead $ u $ fromContext m
  return $ inContext $ snd f a


--------------------------------------------------------------------------------
-- Module
--------------------------------------------------------------------------------
-- Empty programming AST
introModule :: Rule (Context Module)
introModule = minorRule $ makeSimpleRule "Intro module" $ 
  const $ return $ inContext undef

--------------------------------------------------------------------------------
-- Body
--------------------------------------------------------------------------------
introDecls :: Int -> Rule (Context Module)
introDecls = minorRule . toRule "Introduce declarations" undefs . f
  where
    f ndecls = Body_Body noRange [] $ replicate ndecls undef


--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------
introPatternBinding :: Rule (Context Module)
introPatternBinding = toRule "Introduce pattern binding" undefs f
  where 
    f = Declaration_PatternBinding noRange undef undef

introFunctionBindings :: Int -> Rule (Context Module)
introFunctionBindings nbs = minorRule $ toRule "Introduce function binding" undefs f
  where 
    f = Declaration_FunctionBindings noRange $ replicate nbs undef


--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------
introExprNormalApplication :: Int -> Rule (Context Module)
introExprNormalApplication = toRule "Introduce application" undefs . f
  where 
    f nargs = Expression_NormalApplication noRange undef $ replicate nargs undef

introExprInfixApplication :: Bool -> Bool -> Rule (Context Module)
introExprInfixApplication hasLExpr hasRExpr = toRule "Introduce operator" undefs f
  where
    f = Expression_InfixApplication noRange 
                                    (if hasLExpr 
                                       then MaybeExpression_Just undef
                                       else MaybeExpression_Nothing) 
                                    undef
                                    (if hasRExpr 
                                       then MaybeExpression_Just undef
                                       else MaybeExpression_Nothing) 

introExprLet :: Int -> Rule (Context Module)
introExprLet ndecls = toRule "Introduce operator" undefs f
  where
    f = Expression_Let noRange (replicate ndecls undef) undef

introExprLambda :: Int -> Rule (Context Module)
introExprLambda nps = toRule "Introduce operator" undefs f
  where
    f = Expression_Lambda noRange (replicate nps undef) undef

introExprConstructor :: Rule (Context Module)
introExprConstructor = toRule "Intro constructor" undefs f
  where 
    f = Expression_Constructor noRange undef

introExprParenthesized :: Rule (Context Module)
introExprParenthesized = toRule "Intro parentheses" undefs f
  where 
    f = Expression_Parenthesized noRange undef

introExprList :: Int -> Rule (Context Module)
introExprList = ignoreContext . liftRuleInModule . introExprList' 

introExprList' :: Int -> Rule Expression
introExprList' n = makeSimpleRule " Intro expr list" $ \e -> do
   guard (e == undef) 
   return $ Expression_List noRange $ replicate n undef

introExprTuple :: Int -> Rule (Context Module)
introExprTuple = toRule "Intro expr tuple" undefs . f
  where 
    f nexprs = Expression_Tuple noRange $ replicate nexprs undef

-- Variables
introExprVariable :: Rule (Context Module)
introExprVariable = minorRule $ toRule "Intro variable" undefs f
  where 
    f = Expression_Variable noRange undef

-- Literals
introExprLiteral :: Rule (Context Module)
introExprLiteral = minorRule $ toRule "Intro literal" undefs f
  where 
    f = Expression_Literal noRange undef


--------------------------------------------------------------------------------
-- GuardedExpressions
--------------------------------------------------------------------------------
introGuardedExpr :: Rule (Context Module)
introGuardedExpr = toRule "Introduce pattern variable" undefs f
  where
    f = GuardedExpression_GuardedExpression noRange undef undef


--------------------------------------------------------------------------------
-- LeftHandSide
--------------------------------------------------------------------------------
introLHSFun :: Int -> Rule (Context Module)
introLHSFun = toRule "Introduce function name and patterns" undefs . f
  where
    f nps = LeftHandSide_Function noRange undef $ replicate nps undef


--------------------------------------------------------------------------------
-- RightHandSide
--------------------------------------------------------------------------------
introRHSExpr :: Int -> Rule (Context Module)
introRHSExpr ndecls = toRule "Introduce pattern variable" undefs f
  where
    f = RightHandSide_Expression noRange 
                                 undef
                                 (if ndecls > 0
                                    then MaybeDeclarations_Just (replicate ndecls undef)
                                    else MaybeDeclarations_Nothing)

introRHSGuarded :: Int -> Int -> Rule (Context Module)
introRHSGuarded ngexprs ndecls = toRule "Introduce pattern variable" undefs f
  where
    f = RightHandSide_Guarded noRange 
                              (replicate ngexprs undef)
                              (if ndecls > 0
                                 then MaybeDeclarations_Just (replicate ndecls undef)
                                 else MaybeDeclarations_Nothing)



--------------------------------------------------------------------------------
-- Patterns
--------------------------------------------------------------------------------
introPatternVariable :: Rule (Context Module)
introPatternVariable = toRule "Introduce pattern variable" undefs f
  where
    f = Pattern_Variable noRange undef

introPatternConstructor :: Int -> Rule (Context Module)
introPatternConstructor = toRule "Introduce pattern constructor" undefs . f
  where
    f nps = Pattern_Constructor noRange undef $ replicate nps undef

introPatternInfixConstructor :: Rule (Context Module)
introPatternInfixConstructor = toRule "Introduce infix pattern constructor" undefs f
  where
    f = Pattern_InfixConstructor noRange undef undef undef

introPatternParenthesized :: Rule (Context Module)
introPatternParenthesized = toRule "Introduce pattern parentheses" undefs f
  where
    f = Pattern_Parenthesized noRange undef

introPatternLiteral :: Rule (Context Module)
introPatternLiteral = toRule "Introduce literal pattern" undefs f
  where
    f = Pattern_Literal noRange undef

introPatternTuple :: Int -> Rule (Context Module)
introPatternTuple nps = toRule "Introduce tuple pattern" undefs f
  where
    f = Pattern_Tuple noRange $ replicate nps undef

introPatternWildcard :: Rule (Context Module)
introPatternWildcard = toRule "Introduce wildcard pattern" undefs f
  where
    f = Pattern_Wildcard noRange

--------------------------------------------------------------------------------
-- Literals
--------------------------------------------------------------------------------
introLiteralInt :: String -> Rule (Context Module)
introLiteralInt = toRule "Introduce a literal integer" undefs . f
  where
    f = Literal_Int noRange

introLiteralString :: String -> Rule (Context Module)
introLiteralString = toRule "Introduce a literal string" undefs . f
  where
    f = Literal_String noRange

--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------
introNameIdentifier :: String -> Rule (Context Module)
introNameIdentifier = toRule "Introduce identifier name" undefs . f
  where
    f = Name_Identifier noRange []

introNameOperator :: String -> Rule (Context Module)
introNameOperator = toRule "Introduce operator name" undefs . f
  where
    f = Name_Operator noRange []

introNameSpecial :: String -> Rule (Context Module)
introNameSpecial = toRule "Introduce special name" undefs . f
  where
    f = Name_Special noRange []


-- help functions
range :: (Int, Int) -> Range
range (line, col) = Range_Range (Position_Position "" line col) Position_Unknown


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
