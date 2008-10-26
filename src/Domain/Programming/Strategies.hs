module Domain.Programming.Strategies where

import Prelude hiding (sequence)
import Common.Context
import Common.Strategy
import Common.Uniplate
import Common.Exercise
import Common.Transformation
import Common.Apply
import Common.Parsing (SyntaxError(..))
import Domain.Programming.Expr
import Domain.Programming.Rules
import Domain.Programming.Prelude (isortE2,insertE)
import Data.Maybe
import Data.Char

-- strategies derived from the abstract syntax of expressions

getStrategy :: Expr -> Strategy (Context Expr)
getStrategy expr = 
   case expr of
      Lambda x e -> introLambda x <*> getStrategy e
      MatchList b n c -> introMatchList <*> getStrategy b <*> getStrategy n <*> getStrategy c
      Var x -> toStrategy (introVar x)
      --Let x b d -> introLet x <*> getStrategy b <*> getStrategy d
      Apply (Lambda f body) (Fix (Lambda g e)) | f==g ->
         introLet f <*> getStrategy e <*> getStrategy body
      Fix (Lambda f e) -> getStrategy (makeLet f e (Var f))
      Apply f a -> introApply <*> getStrategy f <*> getStrategy a
      IfThenElse c t e -> introIf <*> getStrategy c <*> getStrategy t <*> getStrategy e

{- The abstract strategy has a slightly more abstract representation
of list pattern matching. The following function is partial, and
only works on pattern matching over variables. -}
getAbstractStrategy :: Expr -> Strategy (Context Expr)
getAbstractStrategy expr = 
   case expr of
      Lambda x e -> introLambda x <*> getAbstractStrategy e
      MatchList b n c -> listPatternMatching b (getLambda c) (getLambda (dropLambda c)) <*> getAbstractStrategy n <*> getAbstractStrategy (dropLambda (dropLambda c))
      Var x -> toStrategy (introVar x)
      Apply (Lambda f body) (Fix (Lambda g e)) | f==g -> 
         introLet f <*> getAbstractStrategy e <*> getAbstractStrategy body
      Fix (Lambda f e) -> getAbstractStrategy (makeLet f e (Var f))
      Apply f a -> introApply <*> getAbstractStrategy f <*> getAbstractStrategy a
      IfThenElse c t e -> introIf <*> getAbstractStrategy c <*> getAbstractStrategy t <*> getAbstractStrategy e
      
getLambda (Lambda x e)   = x
getLambda expr           = error "Not a lambda"

dropLambda (Lambda x e)  = e
dropLambda expr          = error "Not a lambda"
 
-- the insertion sort strategy with a fold (Bastiaan)

isortStrategy :: Strategy (Context Expr)
isortStrategy = getStrategy isortE2

isortAbstractStrategy :: Strategy (Context Expr)
isortAbstractStrategy = getAbstractStrategy isortE2

-- the insertion sort strategy with a fold (Johan)

{- the fold & para strategies
-- These strategy are only partially typed: the type of the arguments is
-- a strategy. Maybe we can obtain more type info in the arguments?
-- Furthermore, we have to implement the different ways to construct a
-- foldr/para.
--
-- I think we need to define something like this in order to implement
-- different ways to implement folds.
-}

foldS :: Strategy (Context Expr) -> Strategy (Context Expr) -> Strategy (Context Expr)
foldS consS nilS = toStrategy (introVar "foldr") <*> consS <*> nilS

paraS :: Strategy (Context Expr) -> Strategy (Context Expr) -> Strategy (Context Expr)
paraS consS nilS = toStrategy (introVar "para") <*> consS <*> nilS

-- Using fold and para

isortFoldStrategy :: Strategy (Context Expr)
isortFoldStrategy = foldS insertS nilS

insertS = getStrategy insertE
nilS = toStrategy (introVar "nil")

insertS2 = introLambda "a" <*> paraS insertConsS insertNilS
  where insertNilS = undefined -- gestrategy on the cooresponding expression
        insertConsS = undefined -- idem


run = apply isortStrategy (inContext undef)
