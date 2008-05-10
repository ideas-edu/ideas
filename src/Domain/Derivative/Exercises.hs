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
import Domain.Derivative.Basic
import Domain.Derivative.Rules
import Common.Apply
import Common.Exercise
import Common.Context
import Common.Strategy
import Data.Char
import Data.Maybe

derivativeExercise :: Exercise (Context Function)
derivativeExercise = makeExercise
   { shortTitle    = "Derivative"
   , parser        = \input -> case reads input of
                                  [(fun, rest)] | all isSpace rest -> Right (inContext fun)
                                  _ -> Left (text "not a function")
   , finalProperty = noDerivative . fromContext
   , ruleset       = derivativeRules ++ tidyupRules
   , strategy      = derivativeStrategy
   }
   
derivativeStrategy :: LabeledStrategy (Context Function)
derivativeStrategy = label "Derivative" $
   tidyup <*> repeat (derivative <*> tidyup)

tidyup :: Strategy (Context Function)
tidyup = repeat $ somewhere $ alternatives tidyupRules

derivative :: Strategy (Context Function)
derivative = somewhere $ alternatives derivativeRules

ex :: Function
ex = (Con (1/3) :*: (x :^: Con 3)) :+: (Con (-3) :*: (x :^: Con 2)) :+: x :+: (Con (-5))
 where x = Var "x"

test :: Function -> Function
test = fromContext . fromJust . apply derivativeStrategy . inContext . Derivative "x"

q1 = test ex
q2 = test q1
q3 = test q2
q4 = test q3