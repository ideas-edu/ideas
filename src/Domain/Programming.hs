module Domain.Programming where

import Prelude hiding (sequence)
import Common.Context
import Common.Strategy
import Common.Uniplate
import Common.Exercise
import Common.Transformation
import Common.Apply
import Data.Maybe
import Data.Char
import qualified Service.TypedAbstractService as TAS

isort :: Ord a => [a] -> [a]
isort = \xs -> matchList xs
                  nil
                  (\y -> \ys -> 
                     let insert :: Ord a => a -> [a] -> [a]
                         insert = \a -> \xs -> matchList xs 
                                                  ((cons a) nil)
                                                  (\y -> \ys -> (cons y) ((insert a) ys))
                     in (insert y) (isort ys))

isortExpr :: Expr
isortExpr = Lambda "xs" $ MatchList (Var "xs")
                             (Var "nil")
                             (Lambda "y" $ Lambda "ys" $
                                Let "insert" (Lambda "a" $ Lambda "xs" $ MatchList (Var "xs") 
                                                                           (Apply (Apply (Var "Cons") (Var "a")) (Var "Nil"))
                                                                           (Lambda "y" $ Lambda "ys" $ Apply (Apply (Var "cons") (Var "y")) (Apply (Apply (Var "insert") (Var "a")) (Var "ys"))))
                                (Apply (Apply (Var "insert") (Var "y")) (Apply (Var "isort") (Var "ys"))))

matchList :: [a] -> b -> (a -> [a] -> b) ->  b
matchList []     nil _    = nil
matchList (x:xs) _   cons = cons x xs

nil :: [a]
nil = []

cons :: a -> [a] -> [a]
cons = (:)

data Expr = Lambda String Expr
          | MatchList Expr{-body-} Expr{-nil-} Expr{-cons-}
          | Var String
          | Let String Expr Expr
          | Apply Expr Expr
   deriving (Show, Eq, Read)

instance Uniplate Expr where
   uniplate expr =
      case expr of
         Lambda x e -> ([e], \[e] -> Lambda x e)
         MatchList b n c -> ([b,n,c], \[b,n,c] -> MatchList b n c)
         Var _ -> ([], \[] -> expr)
         Let x b d -> ([b,d], \[b,d] -> Let x b d)
         Apply f a -> ([f,a], \[f,a] -> Apply f a)
         
undef :: Expr
undef = Var "undefined"

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

getStrategy :: Expr -> Strategy (Context Expr)
getStrategy expr = 
   case expr of
      Lambda x e -> introLambda x <*> getStrategy e
      MatchList b n c -> introMatchList <*> getStrategy b <*> getStrategy n <*> getStrategy c
      Var x -> toStrategy (introVar x)
      Let x b d -> introLet x <*> getStrategy b <*> getStrategy d
      Apply f a -> introApply <*> getStrategy f <*> getStrategy a

buildExpr :: [Rule (Context Expr)] -> Expr
buildExpr = fromContext . foldl (flip applyD) (inContext undef)

applyRule :: Expr -> (Expr -> Maybe Expr) -> Maybe Expr
applyRule e f = somewhereM f e

testje1 = isortExpr == buildExpr (getRules isortExpr)
testje2 = isortExpr == fromContext (applyD isortStrategy (inContext undef))

isortExercise :: Exercise Expr
isortExercise = Exercise   
   { shortTitle    = "Insertion sort"
   , parser        = \s -> case reads s of  
                             [(a, rest)] | all isSpace rest -> Right a 
                             _ -> Left "parse error"
   , subTerm       = \_ _ -> Nothing
   , prettyPrinter = show
   , equivalence   = (==)
   , equality      = (==)
   , finalProperty = const True
   , ruleset       = []
   , strategy      = label "isort" isortStrategy
   , generator     = return undef
   , suitableTerm  = const True
   }
   
isortStrategy :: Strategy (Context Expr)
isortStrategy = getStrategy isortExpr

toRule :: String -> (Expr -> Maybe Expr) -> Rule (Context Expr)
toRule s f = liftRuleToContext $ makeSimpleRule s (\e -> applyRule e f)

run = apply isortStrategy (inContext undef)

{-
Lambda "xs" (MatchList (Var "xs") (Var "nil") (Lambda "y" (Lambda "ys" (Let "insert" (Lambda "a" (Lambda "xs" (MatchList (Var "xs") (Apply (Apply (Var "Cons") (Var "a")) (Var "Nil")) (Lambda "y" (Lambda "ys" (Apply (Apply (Var "cons") (Var "y")) (Apply (Apply (Var "insert") (Var "a")) (Var "ys")))))))) (Apply (Apply (Var "insert") (Var "y")) (Apply (Var "isort") (Var "ys")))))))
-}

{-
test = do 
   s0 <- TAS.generate isortExercise 0
   let s1 = head $ TAS.allfirsts s0
   let list = iterate (\(_,_,s) -> head $ TAS.allfirsts s) s1
   putStrLn $ unlines $ map show $ take 35 list -}