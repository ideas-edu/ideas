module Domain.Programming.Prog where

import Common.Context
import Common.Grammar
import Common.Strategy
import Data.Generics.Biplate
import Data.Generics.PlateData
import Data.Data hiding (Fixity)
import Data.List hiding (union, lookup)
import Domain.Programming.AlphaConv (alphaConversion)
import Domain.Programming.Strategies (getRules)
import Domain.Programming.Helium

{- ideas:

   o  rewrite 

   o  Use a GADT to get the strategies typed again

   o  Change show function for names, show also the name of the constructor.
      For the range datatype: remove from show.

   o  Devise a scheme in which certain strategies, like etaS and parenS, can
      be used at any point in the strategy.

   o  Add beta reduction, uniplate for app (\x->expr) y

   o  Add etaS to prelude strategies
-}

-- Test values
(Right m) = compile "f x = x + 3"

--------------------------------------------------------------------------------
-- Help functions
--------------------------------------------------------------------------------
collectNames :: Module -> [String]
collectNames m = nub [ s | Name_Identifier _ _ s <- universeBi m ]

freshVarStrings :: Module -> [String]
freshVarStrings = (['x' : show i | i <- [1..]] \\) . collectNames

-- allSolutions strat = map (fromContext . snd . last . snd) $ derivations strat $ inContext emptyProg
isSolution strat m = member m (noLabels strat)

checkExercise :: Strategy (Context Module) -> String -> IO ()
checkExercise strat x = putStrLn $ x ++ " : " ++ 
                        show (isSolution strat (getRules (fromRight (compile x))))
  where
    fromRight x = case x of
                    Right y -> y
                    _       -> error "no compile"
checkExercises :: Strategy (Context Module) -> [String] -> IO ()
checkExercises strat xs = mapM_ (checkExercise strat) xs


--------------------------------------------------------------------------------
-- Equality: rewrite to normal form and check syntatically (cannot be solved generally)
--------------------------------------------------------------------------------

equalModules :: Module -> Module -> Bool
equalModules x y = normalise x == normalise y

normalise :: Module -> Module
normalise = alphaRenaming . removeRanges
    
removeRanges = transformBi (\(Range_Range  _ _) -> noRange)

-- eta reduction, e.g. \x -> f x => f
etaReduction :: Module -> Module
etaReduction = transformBi red
  where
    red x = case x of 
      Expression_Lambda _ [Pattern_Variable _ p] 
        (Expression_NormalApplication _ f [Expression_Variable a]) -> if p == n 
                                                                      then f
                                                                      else x 
      _                                                            -> x

{-

rewrite functions e.g. f x = div x => f = div

-- wil do this strat. when needed
etaFunS :: ModuleS -> ModuleS -> ModuleS
etaFunS name expr =  introFunctionBindings 1 <*> introLHSFun 1 <*> name <*> introPatternVariable <*>
                     introNameIdentifier "x" <*> introRHSExpr 0 <*> introExprNormalApplication 1 <*> expr <*>
                     introExprVariable <*> introNameIdentifier "x"
                 <|> introPatternBinding <*> introPatternVariable <*> name <*> introRHSExpr 0 <*> etaS expr
-}

{-
-- rewrite lambda expressions, e.g. \x -> \y -> x + y => \x y -> x + y
lambdaS :: [ModuleS] -> ModuleS -> ModuleS
lambdaS args expr = alternatives $ map f $ split args
  where
    f (xs, ys) = introExprLambda (length xs) <*> sequence xs <*> rec ys
    rec ys = case ys of
               [] -> expr
               ps -> lambdaS ps expr



-- beta reduction, e.g. (\x -> x + x) 42 => 42 + 42
betaReductionRule :: Rule (Context Module)
betaReductionRule = makeSimpleRule "Beta reduction" f
  where
    f m = case [() | Expression_NormalApplication _ 
                       (Expression_Parenthesized _ 
                         (Expression_Lambda _ ps expr)) as <- universeBi m'] of
            []      -> Nothing
            redexes -> Just $ inContext $ betaReduce $ m'
      where m' = fromContext m

betaReduce :: Module -> Module
betaReduce = transformBi red
  where
    red x = case x of 
      Expression_NormalApplication _ (Expression_Parenthesized _ 
          (Expression_Lambda _ ps expr)) as -> case drop (length as) ps of
            []  -> substArgs expr ps as
            ps' -> Expression_Lambda noRange ps' (substArgs expr (take (length as) ps) as)
      _                                     -> x 

substArgs :: Data a => a -> Patterns -> Expressions -> a
substArgs expr ps =  foldr transformBi expr . zipWith rep ps
  where   
    rep (Pattern_Variable _ p) x e = case e of
              Expression_Variable _ n -> if n == p then x else e
              _                       -> e
    rep _                      _ e = e


-- application rewrites, e.g. ((f 1) 2) 3 => f 1 2 3
appS :: ModuleS -> [ModuleS] -> ModuleS
appS f args = parenS $ app f args
-- appS f args = fix (\ t -> curryS f args <|> infixS f args <|> etaS t)

-- 1 `f` 2 3 => f 1 2 3
infixS f args = case args of 
                  x:y:z:zs -> app (parenS (infixApp f x y)) (z:zs)
                  x:y:[]   -> infixApp f x y
                  _        -> fail
  where
    infixApp f l r = introExprInfixApplication True True <*> l <*> f <*> r

{-
curryS :: ModuleS -> [ModuleS] -> ModuleS
curryS f args = case args of 
                  []   -> fail
                  a:as -> app f (a:as) <|> partialS f args 
                                       <|> curryS (parenS (app f [a])) as
-}

curryS :: ModuleS -> [ModuleS] -> ModuleS
curryS f args = fix (\t -> case args of 
                             []   -> fail
                             a:as -> app f (a:as) <|> curryS (parenS (app f [a])) as)

funS' :: Int -> ModuleS -> [ModuleS] -> ModuleS
funS' kind f args  =  appS f args
                 <|> if partargs > 0
                     then lambdaS (take partargs [varS ('x' : show i) | i <- [1..]]) (appS f args)
                     else fail
  where partargs = kind - (length args)

partialS :: ModuleS -> [ModuleS] -> ModuleS
partialS f args | length args > 2  = alternatives $ map g $ init $ split args
                | otherwise        = fail
  where
    g (x, y) = curryS (parenS (f `app` x)) y



opS n l r = case (l, r) of 
              (Just x, Just y)   -> app (prefix) [x, y] <|> 
                                    app (pleft x) [y] <|> 
                                    app (pright y) [x] <|> 
                                    infixApp True True <*> x  <*> op <*> y
              (Nothing, Just y)  -> pright y <|> parenS (lambdaS [patS "x"] (opS n (Just (varS "x")) r))
              (Just x, Nothing)  -> pleft x  <|> parenS (lambdaS [patS "x"] (opS n l (Just (varS "x"))))
              (Nothing, Nothing) -> prefix   <|> parenS (lambdaS [patS "x", patS "y"] (opS n (Just (varS "x")) (Just (varS "y"))))
  where 
    infixApp l r = introExprInfixApplication l r
    pleft x = infixApp True  False <*> x  <*> op
    prefix = infixApp False False <*> op
    pright y = infixApp False True  <*> op <*> y

-}

-- drop all parentheses
removeParen :: Module -> Module
removeParen = transformBi f
  where f (Expression_Parenthesized _ expr) = expr
        f x = x