module Domain.Programming.Rules where

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


introLambda :: String -> Rule (Context Expr)
introLambda x = toRule "Intro Lambda" f 
 where
   f e | e == undef = return $ Lambda x undef
   f _ = Nothing

introMatchList :: Rule (Context Expr)
introMatchList = toRule "Intro MatchList" f 
 where
   f e | e == undef = return $ MatchList undef undef undef
   f _ = Nothing

listPatternMatching varname head tail = toRule "List Pattern Matching" f
  where
    f e | e == undef = return $ MatchList varname undef (Lambda head $ Lambda tail $ undef)
    f _ = Nothing

introVar:: String -> Rule (Context Expr)
introVar x = toRule "Intro Var" f 
 where
   f e | e == undef = return $ Var x
   f _ = Nothing

introLet :: String -> Rule (Context Expr)
introLet x = toRule "Intro Let" f 
 where
   f e | e == undef = return $ Let x undef undef
   f _ = Nothing

introApply :: Rule (Context Expr)
introApply = toRule "Intro Apply" f 
 where
   f e | e == undef = return $ Apply undef undef
   f _ = Nothing


getRules :: Expr -> [Rule (Context Expr)]
getRules expr = 
   case expr of
      Lambda x e -> introLambda x : getRules e
      MatchList b n c -> introMatchList : getRules b ++ getRules n ++ getRules c
      Var x -> introVar x : []
      Let x b d -> introLet x : getRules b ++ getRules d
      Apply f a -> introApply : getRules f ++ getRules a


toRule :: String -> (Expr -> Maybe Expr) -> Rule (Context Expr)
toRule s f = liftRuleToContext $ makeSimpleRule s (\e -> applyRule e f)

buildExpr :: [Rule (Context Expr)] -> Expr
buildExpr = fromContext . foldl (flip applyD) (inContext undef)

applyRule :: Expr -> (Expr -> Maybe Expr) -> Maybe Expr
applyRule e f = somewhereM f e
