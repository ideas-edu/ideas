{-# OPTIONS -XStandaloneDeriving #-}
module Domain.Programming.Exercises where

import Domain.Programming.Expr
import Domain.Programming.Parser
import Domain.Programming.Strategies
import Common.Context
import Common.Strategy
import Common.Uniplate
import Common.Exercise
import Common.Transformation
import Common.Apply
import Text.Parsing (SyntaxError(..))
import Common.Rewriting
import Data.Maybe
import Data.Char
import Domain.Programming.Parser
import Domain.Programming.Helium hiding (undef)
import qualified Domain.Programming.Helium as H
import qualified UHA_Pretty as PP (sem_Module) 
import OneLiner
import qualified UHA_OneLine as OL

isortExercise :: Exercise Expr
isortExercise = Exercise   
   { identifier    = "isort"
   , domain        = "programming"
   , description   = "Insertion sort"
   , status        = Experimental
{-   , parser        = \s -> case reads s of  
                             [(a, rest)] | all isSpace rest -> Right a 
                             _ -> Left $ ErrorMessage "parse error" -}
   , parser        = parseExpr
   , subTerm       = \_ _ -> Nothing
   , prettyPrinter = \e -> ppExpr (e,0)
   , equivalence   = \_ _ -> True
   , equality      = (==)
   , finalProperty = const True
   , ruleset       = []
   , strategy      = label "isort"  isortAbstractStrategy
   , differences   = treeDiff
   , ordering      = compare
   , generator     = return undef
   , suitableTerm  = const True
   }

heliumExercise :: Exercise Module
heliumExercise = Exercise   
   { identifier    = "helium"
   , domain        = "programming"
   , description   = "Helium testing"
   , status        = Experimental
   , parser        = \s -> if s == "" then Right emptyProg else modParser s 
   , subTerm       = \_ _ -> Nothing
   , prettyPrinter = show . PP.sem_Module
   , equivalence   = \_ _ -> True
   , equality      = \_ _ -> False
   , finalProperty = const True
   , ruleset       = []
   , strategy      = label "helium" succeed
   , differences   = \_ _ -> [([], Different)]
   , ordering      = \_ _ -> LT
   , generator     = return emptyProg
   , suitableTerm  = const True
   }

modParser s = case compile s of
                Left e  -> Left $ ErrorMessage e
                Right m -> Right m

emptyProg =  Module_Module noRange
                           MaybeName_Nothing
                           MaybeExports_Nothing
                           (Body_Body noRange [] [])

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
