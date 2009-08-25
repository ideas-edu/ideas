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

import Common.Utils (safeHead)
import Common.Context hiding (get)
import Common.Strategy hiding (not)
import Control.Monad.State
import Data.Generics.Biplate
import Data.Generics.PlateData
import Data.Maybe
import Data.Data hiding (Fixity)
import Data.List hiding (union, lookup)
import Domain.Programming.AlphaRenaming (alphaRenaming)
import Domain.Programming.Helium
import Domain.Programming.HeliumRules
import Domain.Programming.Substitute

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
-- Help Functions
--------------------------------------------------------------------------------
collectNames :: Module -> [String]
collectNames m = nub [ s | Name_Identifier _ _ s <- universeBi m ]

freshVarStrings :: Module -> [String]
freshVarStrings = (['x' : show i | i <- [1..]] \\) . collectNames

allSolutions strat = map (fromContext . snd . last . snd) 
                         (derivations strat $ inContext emptyProg)
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
  let percentage = count * 100  `div` toInteger (length es)
  putStrLn $ "\n" ++ take 4 (show percentage) ++ "% has been recognised by the strategy.\n"
  return ()

pat2var :: Pattern -> Expression
pat2var (Pattern_Variable r n) = Expression_Variable r n

var = Expression_Variable noRange

pp = putStrLn . ppModule
comp = (\(Right m)->m) . compile . fst

--------------------------------------------------------------------------------
-- Equality: rewrite to normal form and check syntatically (cannot be solved generally)
--------------------------------------------------------------------------------
equalModules :: Module -> Module -> Bool
equalModules x y = normaliseModule x == normaliseModule y

normaliseModule :: Module -> Module
normaliseModule = alphaRenaming . normalise . alphaRenaming

normalise :: Module -> Module
normalise = rewrites . preprocess
  where
    preprocess = removeRanges . removeParens
    rewrites = rewriteBi $  (etaReduce 
                        >->  betaReduce 
                        >->  lambdaReduce 
                        >->  applicationReduce 
                        >->  infix2prefix 
                        >->  commutativeOps)
                       >>-> (inlineWhere 
                        >->  let2where)
                       >>->  inlineBinding

-- Choice do all rewrites in a Module or just one and let it to the rewriteBi
liftRule :: (Data a, Data b) => (a -> Maybe a) -> b -> Maybe b
liftRule rule m =
   safeHead [ fill a | (h, fill) <- contextsBi m, Just a <- [rule h] ]

-- a kind of Left-to-right Kleisli composition of monads
(>->) :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
f >-> g = \ x -> f x `mplus` g x
infixr 1 >->

(>>->) :: (Data a, Data b) => (a -> Maybe a) -> (b -> Maybe b) -> Module -> Maybe Module
f >>-> g = \x -> (liftRule f) x `mplus` (liftRule g) x
infixr 2 >>->

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
etaReduce expr = 
  case expr of 
    Expression_Lambda _ [Pattern_Variable _ p]  
      (Expression_NormalApplication _ f [Expression_Variable _ v]) -> do
        guard (p == v)
        return f
    Expression_Lambda _ [Pattern_Variable _ p] 
      (Expression_NormalApplication r f args) -> 
        case last args of
          Expression_Variable _ v -> do
            guard (p == v)
            return $ Expression_NormalApplication r f $ init args
          _  -> Nothing
    _ -> Nothing

-- beta reduction, e.g. (\x -> x + x) 42 => 42 + 42
betaReduce :: Expression -> Maybe Expression
betaReduce expr = 
  case expr of 
    Expression_NormalApplication r (Expression_Lambda _ [p] e) (a:as) -> 
      Just $ Expression_NormalApplication r (subst (pat2var p, a) e) as
    _ -> Nothing

-- rewrite lambda expressions, e.g. \x -> \y -> x + y => \x y -> x + y
lambdaReduce :: Expression -> Maybe Expression
lambdaReduce expr = 
  case expr of
    Expression_Lambda r (p:q:qs) e -> Just $ Expression_Lambda r [p]
                                           $ Expression_Lambda r (q:qs) e
    _ -> Nothing

-- application rewrites, e.g. ((f 1) 2) 3 => f 1 2 3
applicationReduce :: Expression -> Maybe Expression
applicationReduce expr = 
  case expr of
    Expression_NormalApplication r  
      (Expression_NormalApplication _ f as) as' -> 
        Just $ Expression_NormalApplication r f (as ++ as')
    Expression_NormalApplication _ f [] -> Just $ f
    _ -> Nothing

-- infix application rewrites, e.g. 1 `div` 2 => div 1 2 or 1 + 2 => (+) 1 2
infix2prefix :: Expression -> Maybe Expression
infix2prefix expr = 
  case expr of
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
commutativeOps expr =
  case expr of
    Expression_NormalApplication r
      (Expression_InfixApplication r' MaybeExpression_Nothing f MaybeExpression_Nothing) args -> do
        guard (not (isSorted args) && (isOp f "+" || isOp f "*")) -- should be done elsewhere, in preludeS?
        return $ Expression_NormalApplication r
                   (Expression_InfixApplication r' MaybeExpression_Nothing 
                                                f MaybeExpression_Nothing) $ sort args
    _ -> Nothing
  where
    isSorted xs = sort xs == xs
    isOp op n = case op of
                  Expression_Variable _ (Name_Operator _ [] n') -> n == n'
                  _ -> False

inlineWhere :: RightHandSide -> Maybe RightHandSide
inlineWhere rhs = 
  case rhs of
    -- A simple pattern binding, eg. f = g 1 2 3
    RightHandSide_Expression r expr (MaybeDeclarations_Just ws) -> do
      guard $ not $ null patterns
      return $ RightHandSide_Expression r (substAll reps expr) $
               if null rest then MaybeDeclarations_Nothing else MaybeDeclarations_Just rest
      where 
        (patterns, rest) = partition (\d -> case d of 
                                              Declaration_PatternBinding _ _ _ -> True
                                              _ -> False) ws
        reps = [(pat2var pat, expr) | p <- patterns, 
                                      Declaration_PatternBinding _ pat 
                                        (RightHandSide_Expression _ expr _) <- universeBi p]
    _ -> Nothing

inlineBinding :: Declaration -> Maybe Declaration -- from fun binding to pat binding
inlineBinding decl =
  case decl of  -- A non recursive function binding, eg. f n = g n ; f 2 = g 3
    Declaration_FunctionBindings _ [fb] -> 
      case fb of
        FunctionBinding_FunctionBinding _ _ (RightHandSide_Expression _ _ MaybeDeclarations_Nothing) ->
          Just $ anonymise' fb
        _ -> Nothing
    _ -> Nothing

anonymise' (FunctionBinding_FunctionBinding r lhs rhs) = 
  Declaration_PatternBinding r (Pattern_Variable r name) rhs'
  where
    (name, patterns) = deLHS lhs
    deLHS lhs = case lhs of 
                  LeftHandSide_Function _ n ps -> (n, ps)
                  LeftHandSide_Infix _ l op r   -> (op, [l, r])
                  LeftHandSide_Parenthesized _ lhs' ps -> let (n, ps') = deLHS lhs' in (n, ps' ++ ps)
    expr = case rhs of 
             RightHandSide_Expression _ e MaybeDeclarations_Nothing -> e
             _ -> error "rhs should not contain wheres and is not defined on guards"
    rhs' = RightHandSide_Expression r (Expression_Lambda r patterns expr) MaybeDeclarations_Nothing

anonymise :: Declaration -> Declaration
anonymise d = 
  case d of
    -- f = 1 : f => f = let f = 1 : f in f 
    d@(Declaration_PatternBinding r p rhs) -> if isRecursive d then toLet p [d] else d
    -- f n [] = 0; f n (x:xs) = 1 + f xs => f = \n -> let f [] = 0; f (_:xs) = 1 + f xs in f
    Declaration_FunctionBindings r fbs ->
      case fbs of
        [FunctionBinding_FunctionBinding r lhs rhs] -> undefined
        f:fs -> undefined

toLet :: Pattern -> Declarations -> Declaration
toLet p ds = let r = noRange in
  Declaration_PatternBinding r p (RightHandSide_Expression r
    (Expression_Let r ds (pat2var p)) MaybeDeclarations_Nothing)

isRecursive :: Declaration -> Bool
isRecursive decl = or $ map isRec uni
  where
    uni = (universeBi decl :: [Declaration])
    isRec decl = case decl of
                   Declaration_PatternBinding _ p rhs -> any (== pat2var p) $ universeBi decl
                   Declaration_FunctionBindings _ fbs -> any (== var (name (head fbs))) $ universeBi fbs
                   _ -> False

name :: FunctionBinding -> Name
name = (\(n, _, _, _) -> n) . decomposeFB

decomposeFB :: FunctionBinding -> (Name, Patterns, Expression, Declarations)
decomposeFB (FunctionBinding_FunctionBinding _ lhs rhs) = (name, ps, expr, ds)
  where
    deLHS l = case l of 
                LeftHandSide_Function _ n ps -> (n, ps)
                LeftHandSide_Infix _ l op r   -> (op, [l, r])
                LeftHandSide_Parenthesized _ lhs' ps -> let (n, ps') = deLHS lhs' in (n, ps' ++ ps)
    (name, ps) = deLHS lhs
    (expr, ds) = case rhs of
                    RightHandSide_Expression _ expr w -> (expr, case w of
                                                                  MaybeDeclarations_Just ds -> ds
                                                                  _                         -> [])
                    RightHandSide_Guarded _ gexpr w   -> error "Todo: guarded expressions"

let2where :: RightHandSide -> Maybe RightHandSide
let2where x = 
  case x of
    RightHandSide_Expression r (Expression_Let _ decls expr) maybeDecls -> 
      case maybeDecls of
        MaybeDeclarations_Just ds -> Just $ rhs r expr $ ds ++ decls
        MaybeDeclarations_Nothing -> Just $ rhs r expr decls
    _ -> Nothing
  where
   rhs r expr ds = RightHandSide_Expression r expr $ MaybeDeclarations_Just $ ds


-- detect fixed arguments, eg. f n [] = ... ; f n (x:xs) = ...


-- rewrite functions so that the have no fixed arguments, eg. f n = g  where g [] = ... ; g (x:xs) = ... or
-- let g [] = ... ; g (x:xs) = ... g xs ... ; in f n = g

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