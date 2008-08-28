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
module Domain.Derivative.Exercises where

import Prelude hiding (repeat)
import Domain.Derivative.Rules (derivativeRules, tidyupRules, tidyRule)
import Common.Strategy (Strategy, somewhere, (<*>), alternatives, label, LabeledStrategy)
import qualified Common.Strategy
import Common.Context (Context, liftRuleToContext, inContext, fromContext)
import Common.Exercise (Exercise(..), text)
import OpenMath.Object
import Data.Ratio
import Domain.Derivative.Basic
import Data.Char (isSpace)

derivativeExercise :: Exercise (Context Expr)
derivativeExercise = Exercise
   { shortTitle    = "Derivative"
   , parser        = \input -> case reads input of
                                  [(fun, rest)] | all isSpace rest -> Right (inContext fun)
                                  _ -> Left (text "not a function")
   , equivalence = undefined
   , equality    = (==)
   , suitableTerm = const True
   , subTerm = undefined
   , prettyPrinter = show
   , finalProperty = noDiff . fromContext
   , ruleset       = derivativeRules ++ tidyupRules
   , strategy      = derivativeStrategy
   , generator     = return $ inContext $ Diff $ Lambda "x" $ Var "x" :^: 2
   }

derivativeStrategy :: LabeledStrategy (Context Expr)
derivativeStrategy = label "Derivative" $
   tidyup <*> Common.Strategy.repeat (derivative <*> tidyup)

tidyup :: Strategy (Context Expr)
tidyup = Common.Strategy.repeat $ somewhere (liftRuleToContext tidyRule) --  $ alternatives tidyupRules

derivative :: Strategy (Context Expr)
derivative = somewhere $ alternatives derivativeRules
{-
ex :: Expr
ex = (Con (1/3) :*: (x :^: Con 3)) :+: (Con (-3) :*: (x :^: Con 2)) :+: x :+: (Con (-5))
 where x = Var "x"

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
fromExpr e = error $ "Unknown Expr: " ++ show e
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