-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  unknown
--
--
-----------------------------------------------------------------------------

module Domain.Programming.Prog where

import Common.Context hiding (get)
import Common.Strategy
import Control.Monad.State
import Data.Generics.Biplate
import Data.Generics.PlateData
import Data.Data hiding (Fixity)
import Data.List hiding (union, lookup)
import Domain.Programming.AlphaRenaming (alphaRenaming)
import Domain.Programming.Helium
import Domain.Programming.HeliumRules

{- ideas:

   o  Use a GADT to get the strategies typed again
-}

-- Test values
(Right m) = compile "f x y = x + y"
(Right m2) = compile "f = \\ y -> \\x -> div x y"
(Right m3) = compile "f a b c d = a ; g = ((f 1) 2 3) ; h = g 4"
(Right m3') = compile "f a b c d = a ; g = f 1 2 3 ; t = g 4"
(Right m4) = compile "f x y = x ; g x y = f x y where f x y = y"
(Right m5) = compile "f x y = x ; g x y = f x y"
(Right m6) = compile "f x = g x where g y = reverse y"


--------------------------------------------------------------------------------
-- Help functions
--------------------------------------------------------------------------------
collectNames :: Module -> [String]
collectNames m = nub [ s | Name_Identifier _ _ s <- universeBi m ]

freshVarStrings :: Module -> [String]
freshVarStrings = (['x' : show i | i <- [1..]] \\) . collectNames

allSolutions strat = map (fromContext . snd . last . snd) $ derivations strat $ inContext emptyProg
normalisedSolutions strat =  map normaliseModule $ allSolutions strat
isSolution strat m = elem (normaliseModule m) $ normalisedSolutions strat

checkExercise :: Strategy (Context Module) -> [Module] -> (String, String) -> StateT Integer IO ()
checkExercise strat solutions (solution, name) = do
    correctCount <- get
    let m = compilation
    let isSolution = m `elem` solutions
    liftIO $ putStrLn $ name ++ " : " ++ show isSolution
    put $ if isSolution then correctCount + 1 else correctCount
  where
    compilation = case compile solution of
                    Right y -> normaliseModule y
                    _       -> error $ "no compile: " ++ name

checkExercises :: Strategy (Context Module) -> [(String, String)] -> IO ()
checkExercises strat es = do
  let solutions = normalisedSolutions strat
  (_, count) <- runStateT (mapM (checkExercise strat solutions) es) 0
  let percentage = (fromInteger count) / (fromInteger (toInteger (length es))) * 100.0
  putStrLn $ "\n" ++ take 4 (show percentage) ++ "% has been recognised by the strategy.\n"
  return ()

--------------------------------------------------------------------------------
-- Equality: rewrite to normal form and check syntatically (cannot be solved generally)
--------------------------------------------------------------------------------
equalModules :: Module -> Module -> Bool
equalModules x y = normaliseModule x == normaliseModule y

normaliseModule :: Module -> Module
normaliseModule = alphaRenaming . normalise

normalise :: Data a => a -> a
normalise = rewriteExprs . rewriteRHSs . preprocess
  where
    preprocess = removeRanges . removeParens
    rewriteExprs = rewriteBi $ etaReduce >-> betaReduce >-> lambdaReduce >-> applicationReduce >->
                               infix2prefix >-> commutativeOps 
    rewriteRHSs = rewriteBi inline
    
-- a kind of Left-to-right Kleisli composition of monads
(>->) :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
f >-> g = \ x -> case f x of
                   Just y  -> case g y of
                                Just z  -> Just z
                                Nothing -> Just y -- as long as one of the two rules fires, return a Just
                   Nothing -> g x
infixr 1 >->

removeParens :: Data a => a -> a
removeParens = transformBi f . transformBi g
  where f (Expression_Parenthesized _ expr) = expr
        f x = x
        g (Pattern_Parenthesized _ pat) = pat
        g x = x

removeRanges :: Data a => a -> a
removeRanges = transformBi (\(Range_Range  _ _) -> noRange)


-- eta reduction, e.g. \x -> f x => f
etaReduce :: Expression -> Maybe Expression
etaReduce x = case x of 
  Expression_Lambda _ [Pattern_Variable _ p] 
    (Expression_NormalApplication _ f [Expression_Variable _ v]) -> if p == v
                                                                    then Just f
                                                                    else Nothing
  Expression_Lambda _ [Pattern_Variable _ p] 
    (Expression_NormalApplication r f args)                      -> appLambda
    where
      appLambda = case last args of
                    Expression_Variable _ v -> if p == v
                                               then Just $ Expression_NormalApplication r f $ init args
                                               else Nothing
                    _                       -> Nothing
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
  Expression_Lambda r [p] expr    -> Nothing
  Expression_Lambda r (p:ps) expr -> Just $ Expression_Lambda r [p] $ Expression_Lambda r ps expr
  _                               -> Nothing

-- application rewrites, e.g. ((f 1) 2) 3 => f 1 2 3
applicationReduce :: Expression -> Maybe Expression
applicationReduce x = case x of
  Expression_NormalApplication r  
    (Expression_NormalApplication _ f as) as' -> Just $ Expression_NormalApplication r f (as ++ as')
  Expression_NormalApplication r f []         -> Just $ f
  _                                           -> Nothing

-- infix application rewrites, e.g. 1 `div` 2 => div 1 2 or 1 + 2 => (+) 1 2
infix2prefix :: Expression -> Maybe Expression
infix2prefix x = case x of
  Expression_InfixApplication _ l f r ->
      case (l, r) of
        (MaybeExpression_Nothing, MaybeExpression_Nothing) -> Nothing
        (MaybeExpression_Just x, MaybeExpression_Nothing)  -> Just $ app (op f) [x]
        (MaybeExpression_Nothing, MaybeExpression_Just x)  -> Just $ app (op f) [x]
        (MaybeExpression_Just x, MaybeExpression_Just y)   -> Just $ app (op f) [x, y]
  _ -> Nothing
  where
    op f = Expression_InfixApplication noRange MaybeExpression_Nothing f MaybeExpression_Nothing
    app = Expression_NormalApplication noRange


-- quick'n dirty domain specific rewrite rules
commutativeOps :: Expression -> Maybe Expression
commutativeOps x = case x of
  Expression_NormalApplication r
    (Expression_InfixApplication r' MaybeExpression_Nothing f MaybeExpression_Nothing) args ->
      if isSorted args
      then Nothing
      else Just $ Expression_NormalApplication r
                    (Expression_InfixApplication r' MaybeExpression_Nothing f MaybeExpression_Nothing) $ sort args
  _ -> Nothing
  where
    isSorted xs = sort xs == xs
      
inline :: RightHandSide -> Maybe RightHandSide
inline x = case x of
  RightHandSide_Expression r expr 
    (MaybeDeclarations_Just ws) -> Just $ RightHandSide_Expression r (replace expr ws) MaybeDeclarations_Nothing
  _                             -> Nothing
  where 
    replace expr = foldr transformBi expr . map rep . reps
      where
        reps ws = [(pat, expr) | w <- ws, Declaration_PatternBinding _ pat (RightHandSide_Expression _ expr _) <- universeBi w]
        rep (Pattern_Variable _ p, x) e = case e of
                  Expression_Variable _ n -> if n == p then x else e
                  _                       -> e
        rep _                         e = e
    

{-
-- rewrite functions e.g. f x = div x => f = div
etaFunS :: ModuleS -> ModuleS -> ModuleS
etaFunS name expr =  introFunctionBindings 1 <*> introLHSFun 1 <*> name <*> introPatternVariable <*>
                     introNameIdentifier "x" <*> introRHSExpr 0 <*> introExprNormalApplication 1 <*> expr <*>
                     introExprVariable <*> introNameIdentifier "x"
                 <|> introPatternBinding <*> introPatternVariable <*> name <*> introRHSExpr 0 <*> etaS expr
-}



--------------------------------------------------------------------------------
-- QuickCheck properties
--------------------------------------------------------------------------------

-- Do contract checking for normalised programs