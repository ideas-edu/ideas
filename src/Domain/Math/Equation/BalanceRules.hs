-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.Equation.BalanceRules
   ( plusRule, minusRule, timesRule, divisionRule
   ) where

import Common.Library
import Control.Monad
import Domain.Math.Data.Relation
import Domain.Math.Expr
import Domain.Math.Numeric.Views

plusRule :: Functor f => Parameterized Expr (Transformation (f Expr))
plusRule = parameter1 (makeBinding "term") $ \a -> 
   makeTrans $ Just . fmap (:+: a)

minusRule :: Functor f => Parameterized Expr (Transformation (f Expr))
minusRule = parameter1 (makeBinding "term") $ \a -> 
   makeTrans $ Just . fmap (:-: a)
   
timesRule :: Functor f => Parameterized Expr (Transformation (f Expr))
timesRule = parameter1 (makeBinding "factor") $ \a -> 
   makeTrans $ unlessZero a . fmap (a :*:)

divisionRule :: Parameterized Expr (Transformation (Equation Expr))
divisionRule = parameter1 (makeBinding "factor") $ \a ->
   makeTrans $ unlessZero a . fmap (:/: a)
   
unlessZero :: Expr -> a -> Maybe a
unlessZero e a = do
   r <- matchM rationalView e
   guard (r /= 0)
   return a