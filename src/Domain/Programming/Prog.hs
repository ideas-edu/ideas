module Domain.Programming.Prog where

import Common.Context
import Common.Grammar
import Common.Strategy
import Data.Generics.Biplate
import Data.Generics.PlateData
import Data.Data hiding (Fixity)
import Data.List hiding (union, lookup)
import Domain.Programming.AlphaRenaming (alphaRenaming)
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
(Right m) = compile "f = \\ y -> (\\x -> div x) y"
(Right m2) = compile "f = \\ y -> \\x -> div x y"
(Right m3) = compile "f a b c d = a ; g = ((f 1) 2 3) ; h = g 4"

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
normalise = rewriteBi rules . preprocess
  where
    preprocess = alphaRenaming . removeRanges . removeParens
    rules = etaReduce >-> betaReduce >-> lambdaReduce >-> applicationReduce
    
-- a kind of Left-to-right Kleisli composition of monads
(>->) :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
f >-> g = \ x -> case f x of
                   Just y  -> case g y of
                                Just z  -> Just z
                                Nothing -> Just y -- as long as one of the two rules fires, return a Just
                   Nothing -> g x
infixr 1 >->

removeParens = transformBi f
  where f (Expression_Parenthesized _ expr) = expr
        f x = x

removeRanges = transformBi (\(Range_Range  _ _) -> noRange)


-- eta reduction, e.g. \x -> f x => f
etaReduce :: Expression -> Maybe Expression
etaReduce x = case x of 
  Expression_Lambda _ [Pattern_Variable _ p] 
    (Expression_NormalApplication _ f [Expression_Variable _ v]) -> if p == v
                                                                    then Just f
                                                                    else Nothing
  _                                                              -> Nothing

-- beta reduction, e.g. (\x -> x + x) 42 => 42 + 42
betaReduce :: Expression -> Maybe Expression
betaReduce x = case x of 
  Expression_NormalApplication _ 
    (Expression_Lambda _ ps expr) as -> case drop (length as) ps of
                                         []  -> Just $ substArgs expr ps as
                                         ps' -> Just $ Expression_Lambda noRange ps' 
                                                         (substArgs expr (take (length as) ps) as)
  _                                   -> Nothing

substArgs :: Data a => a -> Patterns -> Expressions -> a
substArgs expr ps =  foldr transformBi expr . zipWith rep ps
  where   
    rep (Pattern_Variable _ p) x e = case e of
              Expression_Variable _ n -> if n == p then x else e
              _                       -> e
    rep _                      _ e = e

-- rewrite lambda expressions, e.g. \x -> \y -> x + y => \x y -> x + y
lambdaReduce :: Expression -> Maybe Expression
lambdaReduce x = case x of
  Expression_Lambda r ps 
    (Expression_Lambda _ ps' expr) -> Just $ Expression_Lambda r (ps ++ ps') expr -- important that alphaRenaming has been done!
  _                                -> Nothing

-- application rewrites, e.g. ((f 1) 2) 3 => f 1 2 3
applicationReduce :: Expression -> Maybe Expression
applicationReduce x = case x of
  Expression_NormalApplication r  
    (Expression_NormalApplication _ f as) as' -> Just $ Expression_NormalApplication r f (as ++ as')
  _                                           -> Nothing

{-
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

curryS :: ModuleS -> [ModuleS] -> ModuleS
curryS f args = case args of 
                  []   -> fail
                  a:as -> app f (a:as) <|> partialS f args 
                                       <|> curryS (parenS (app f [a])) as
-}

{-
-- rewrite functions e.g. f x = div x => f = div
etaFunS :: ModuleS -> ModuleS -> ModuleS
etaFunS name expr =  introFunctionBindings 1 <*> introLHSFun 1 <*> name <*> introPatternVariable <*>
                     introNameIdentifier "x" <*> introRHSExpr 0 <*> introExprNormalApplication 1 <*> expr <*>
                     introExprVariable <*> introNameIdentifier "x"
                 <|> introPatternBinding <*> introPatternVariable <*> name <*> introRHSExpr 0 <*> etaS expr



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



--------------------------------------------------------------------------------
-- QuickCheck properties
--------------------------------------------------------------------------------

