{-# OPTIONS -fno-case-merge #-}
-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.DerivativeExercise where

import Common.Apply
import Common.Uniplate (universe)
import Prelude hiding (repeat)
import Domain.Math.DerivativeRules 
import Common.Strategy (Strategy, somewhere, (<*>), alternatives, label, LabeledStrategy, try)
import qualified Common.Strategy
import Common.Context (Context, liftRuleToContext, inContext, fromContext)
import Common.Exercise
import Common.Transformation
import Test.QuickCheck hiding (label)
import Domain.Math.Expr
import Domain.Math.SExpr
import Domain.Math.Parser

derivativeExercise :: Exercise Expr
derivativeExercise = makeExercise
   { identifier    = "derivative"
   , domain        = "math"
   , description   = "Derivative"
   , status        = Experimental
   , parser        = parseExpr
   , equivalence = (==) -- ??
   , equality    = (==)
   , suitableTerm = const True
   , subTerm = undefined
   , prettyPrinter = show
   , finalProperty = noDiff
   , ruleset       = map liftRuleToContext derivativeRules ++ [tidyup]
   , strategy      = derivativeStrategy
   , generator     = oneof $ map return [ex1, ex2, ex3, ex4]
   }
   
noDiff :: Expr -> Bool
noDiff e = null [ () | Sym "Diff" _ <- universe e ]   

derivativeStrategy :: LabeledStrategy (Context Expr)
derivativeStrategy = label "Derivative" $
   try tidyup <*> Common.Strategy.repeat (derivative <*> try tidyup)

tidyup :: Rule (Context Expr)
tidyup = liftRuleToContext $ makeSimpleRule "Tidy-up rule" $ \old -> 
   let new = toExpr $ (simplifyExpr :: Expr -> SExpr) old
   in if old==new then Nothing else Just new
   
derivative :: Strategy (Context Expr)
derivative = somewhere $ alternatives (map liftRuleToContext derivativeRules)

ex1, ex2, ex3 :: Expr
ex1 = diff $ lambda (Var "x") $ Var "x" `pow` 2
ex2 = diff $ lambda (Var "x") $ ((1/3) :*: (x `pow` fromInteger 3)) :+: (fromInteger (-3) :*: (x `pow` fromInteger 2)) :+: x :+: (fromInteger (-5))
 where x = Var "x"
ex3 = diff $ lambda (Var "x") (2 * Var "x") 
ex4 = diff $ lambda (Var "x") (ln (Var "x"))
test = fromContext $ applyD derivativeStrategy (inContext ex2)

{-
test :: Expr -> Expr
test = fromContext . fromJust . apply derivativeStrategy . inContext . Diff . Lambda "x"

testje :: Expr -> Expr
testje = fromContext . fromJust . apply derivativeStrategy . inContext

q1 = test ex
q2 = test q1
q3 = test q2
q4 = test q3

ex1 = {- Diff "x" -} (Special Sin (Var "x" :^: 2))
ex2 = {- Diff "x" -} (Special Ln (Var "x" - 1))
ex3 = 5 * (((Var "x" :^: 6) + (2 :*: Var "x")) :^: (-1/2))


main = do
   input <- readFile "test/activemath/diff1.txt"
   case parseXML input of
      Left err  -> error (show err)
      Right xml ->
         case xml2omobj xml of
            Left err  -> error (show err)
            Right obj -> print $ fromExpr $ toExpr $ fromExpr $ testje $ toExpr obj
-}
{-
toExpr :: OMOBJ -> Expr
toExpr (OMA [OMS "calculus1" "diff", x]) = Diff (toExpr x)
toExpr (OMBIND (OMS "fns1" "lambda") xs e) = foldr Lambda (toExpr e) xs
toExpr (OMA [OMS "transc1" "sin", x]) = Special Sin (toExpr x)
toExpr (OMA [OMS "transc1" "cos", x]) = Special Cos (toExpr x)
toExpr (OMA [OMS "transc1" "ln", x]) = Special Ln (toExpr x)
toExpr (OMA [OMS "arith1" "power", x, y]) = toExpr x :^: toExpr y
toExpr (OMA [OMS "arith1" "minus", x, y]) = toExpr x :-: toExpr y
toExpr (OMA [OMS "arith1" "plus", x, y]) = toExpr x :+: toExpr y
toExpr (OMA [OMS "arith1" "divide", x, y]) = toExpr x :/: toExpr y
toExpr (OMV x) = Var x
toExpr (OMI n) = Con (fromIntegral n)
toExpr omobj = error $  "Unknown object: " ++ show omobj
      
fromExpr :: Expr -> OMOBJ
fromExpr (Diff x) = OMA [OMS "calculus1" "diff", fromExpr x]
fromExpr (Lambda x e) = OMBIND (OMS "fns1" "lambda") [x] (fromExpr e)
fromExpr (Special Cos x) = OMA [OMS "transc1" "cos", fromExpr x]
fromExpr (Special Sin x) = OMA [OMS "transc1" "sin", fromExpr x]
fromExpr (Special Ln x) = OMA [OMS "transc1" "ln", fromExpr x]
fromExpr (x :^: y) = OMA [OMS "arith1" "power", fromExpr x, fromExpr y]
fromExpr (x :/: y) = OMA [OMS "arith1" "divide", fromExpr x, fromExpr y]
fromExpr (x :-: y) = OMA [OMS "arith1" "minus", fromExpr x, fromExpr y]
fromExpr (x :+: y) = OMA [OMS "arith1" "plus", fromExpr x, fromExpr y]
fromExpr (Var x) = OMV x
fromExpr (Con r)
   | denominator r == 1 = OMI (numerator r) 
   | otherwise = fromExpr (Con (fromIntegral $ numerator r) :/: Con (fromIntegral $ denominator r))
fromExpr e = error $ "Unknown Expr: " ++ show e -}



{-

 -- ("Derivative","[]","Diff","")
mytest = derivation (derivativeExercise, Just (emptyPrefix $ strategy derivativeExercise), inContext $ e)
 where e = Diff (Lambda "x" (Con 10))
 
bas = runPrefix (emptyPrefix $ strategy derivativeExercise) (inContext e)
 where e = Diff (Lambda "x" (Con 10)) -}
 
 {-
import Common.Apply
import Common.Exercise
import Common.Context
import Common.Strategy
import Service.XML
import Service.TypedAbstractService (derivation)
import OpenMath.Object
import Data.Char
import Data.Maybe
import Data.Ratio -}