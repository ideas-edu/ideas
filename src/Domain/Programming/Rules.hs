module Domain.Programming.Rules where

import Common.Context
import Common.Uniplate
import Common.Transformation
import Common.Uniplate
import Common.Apply
import Domain.Programming.Expr
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad

introLambda :: String -> Rule (Context Expr)
introLambda = liftToContext . makeRule "Intro Lambda" . introLambdaT
{- introLambda x = toRule "Intro Lambda" f
 where
   f e | e == undef = return $ Lambda x undef
   f _ = Nothing -}

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
   f e | e == undef = return $ makeLet x undef undef
   f _ = Nothing

introApply :: Rule (Context Expr)
introApply = toRule "Intro Apply" f 
 where
   f e | e == undef = return $ Apply undef undef
   f _ = Nothing

introIf :: Rule (Context Expr)
introIf = toRule "Intro If" f
 where
   f e | e == undef = return $ IfThenElse undef undef undef
   f _ = Nothing

introFix :: Rule (Context Expr)
introFix = toRule "Intro Fix" f 
 where
   f e | e == undef = return $ Fix undef
   f _ = Nothing
   
getRules :: Expr -> [Rule (Context Expr)]
getRules expr = 
   case expr of
      Lambda x e -> introLambda x : getRules e
      MatchList b n c -> introMatchList : getRules b ++ getRules n ++ getRules c
      Var x -> introVar x : []
      -- Let x b d -> introLet x : getRules b ++ getRules d
      Apply (Lambda f b) (Fix (Lambda g e)) | f==g -> introLet f : getRules b ++ getRules e
      Fix (Lambda x e) -> getRules (makeLet x e (Var "x"))
      Apply f a -> introApply : getRules f ++ getRules a
      IfThenElse c t e -> introIf : getRules c ++ getRules t ++ getRules e
      _ -> error (show expr)


toRule :: String -> (Expr -> Maybe Expr) -> Rule (Context Expr)
toRule s f = liftToContext $ makeSimpleRule s (\e -> applyRule e f)

buildExpr :: [Rule (Context Expr)] -> Expr
buildExpr = fromContext . foldl (flip applyD) (inContext undef)

applyRule :: Expr -> (Expr -> Maybe Expr) -> Maybe Expr
applyRule e f = somewhereM f e

-------------------------------------------------------------
-- Auto variables

-- introduces a fresh identifier for the lambda abstraction
introLambdaAuto :: Rule (Context Expr)
introLambdaAuto = makeSimpleRule "Intro Lambda Auto" f
 where
   f :: Context Expr -> Maybe (Context Expr)
   f ce = do
      a <- currentFocus ce
      guard (a == undef)
      let b = Lambda var undef
      return $ changeFocus (const b) ce
    where 
      used = collectVars (fromContext ce)
      var  = head [ [c] | c <- ['a'..], [c] `notElem` used ]

-- introduces a variable using the De Bruyn convention
introVarDB :: Int -> Rule (Context Expr)
introVarDB n = makeSimpleRule "Intro Lambda Auto" f
 where
   f :: Context Expr -> Maybe (Context Expr)
   f ce = do 
      a   <- currentFocus ce
      guard (a == undef)
      var <- findNameDB n (fromLocation $ location ce) (fromContext ce)
      let b = Var var
      return $ changeFocus (const b) ce

focusUndef :: Rule (Context Expr)
focusUndef = minorRule $ makeSimpleRule "Focus on undefined" f
 where
   f :: Context Expr -> Maybe (Context Expr)
   f ce = case findUndefined (fromContext ce) of
             is:_ -> return (setLocation (makeLocation is) ce)
             _    -> Nothing

focusTop :: Rule (Context Expr)
focusTop = minorRule $ makeSimpleRule "Focus Top" f
 where
   f :: Context Expr -> Maybe (Context Expr)
   f = return . setLocation (makeLocation [])

findUndefined :: Expr -> [[Int]]
findUndefined e
   | e == undef = [[]]
   | otherwise  = [ i:is | (i, c) <- zip [0..] (children e), is <- findUndefined c ]

findNameDB :: Int -> [Int] -> Expr -> Maybe String
findNameDB n loc e = do
   vs <- names loc e
   case splitAt n (reverse vs) of
     (xs, y:_) | all (/=y) xs  -> Just y
     _ -> Nothing
 where
   names :: [Int] -> Expr -> Maybe [String]
   names [] _ = return []
   names (i:is) (Lambda x e) = do
      guard (i==0)
      xs <- names is e
      return (x:xs)
   names (i:is) e = do
      a <- child i e
      names is a

-------------------------------------------------------------
-- Parameterized transformations

introLambdaT :: String -> Transformation Expr
introLambdaT suggested = supplyWith1 descr auto (makeTrans "Intro Lambda" . somewhereM . f)
 where
   descr  = identArgDescr suggested
   auto _ = Just suggested
 
   f :: String -> Expr -> Maybe Expr
   f x e | e == undef = return $ Lambda x undef
   f _ _ = Nothing

identArgDescr :: String -> ArgDescr String
identArgDescr suggested = ArgDescr
   { labelArgument   = "identifier"
   , defaultArgument = Just suggested
   , parseArgument   = \x -> guard (valid x) >> Just x
   , showArgument    = id
   , genArgument     = return suggested
   }
 where
   -- is the String a valid identifier?
   valid :: String -> Bool
   valid []     = False
   valid (x:xs) = isLower x && all isAlphaNum xs