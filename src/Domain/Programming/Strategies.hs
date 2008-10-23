module Domain.Programming.Strategies where

import Prelude hiding (sequence)
import Common.Context
import Common.Strategy
import Common.Uniplate
import Common.Exercise
import Common.Transformation
import Common.Apply
import Common.Parsing (SyntaxError(..))
import Data.Maybe
import Data.Char

getStrategy :: Expr -> Strategy (Context Expr)
getStrategy expr = 
   case expr of
      Lambda x e -> introLambda x <*> getStrategy e
      MatchList b n c -> introMatchList <*> getStrategy b <*> getStrategy n <*> getStrategy c
      Var x -> toStrategy (introVar x)
      Let x b d -> introLet x <*> getStrategy b <*> getStrategy d
      Apply f a -> introApply <*> getStrategy f <*> getStrategy a

{- The abstract strategy has a slightly more abstract representation
of list pattern matching. The following function is partial, and
only works on pattern matching over variables. -}
getAbstractStrategy :: Expr -> Strategy (Context Expr)
getAbstractStrategy expr = 
   case expr of
      Lambda x e -> introLambda x <*> getAbstractStrategy e
      MatchList b n c -> listPatternMatching b (getLambda c) (getLambda (dropLambda c)) <*> getAbstractStrategy n <*> getAbstractStrategy (dropLambda (dropLambda c))
      Var x -> toStrategy (introVar x)
      Let x b d -> introLet x <*> getAbstractStrategy b <*> getAbstractStrategy d
      Apply f a -> introApply <*> getAbstractStrategy f <*> getAbstractStrategy a

getLambda (Lambda x e)   = x
getLambda expr           = error "Not a lambda"

dropLambda (Lambda x e)  = e
dropLambda expr          = error "Not a lambda"
 
isortStrategy :: Strategy (Context Expr)
isortStrategy = getStrategy isortExpr

isortAbstractStrategy :: Strategy (Context Expr)
isortAbstractStrategy = getAbstractStrategy isortExpr

run = apply isortStrategy (inContext undef)
