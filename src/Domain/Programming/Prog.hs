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
import Data.Map (Map, insert, empty, lookup)
import Data.Maybe
import Data.Data hiding (Fixity)
import Data.List hiding (union, lookup, insert)
import Domain.Programming.AlphaRenaming (alphaRenaming)
import Domain.Programming.Helium
import Domain.Programming.HeliumRules
import Domain.Programming.InlinePatternBindings (Env, inlinePatternBindings, updateEnv')
import Domain.Programming.Substitute
import Domain.Programming.Utils
import Prelude hiding (lookup)

{- ideas:

   o  Use a GADT to get the strategies typed again
-}

-- Test values
(Right m') = compile "f x y = x + y"
(Right m2) = compile "f = \\ y -> \\x -> div x y"
(Right m3) = compile "f a b c d = a ; g = ((f 1) 2 3) ; h = g 4"
(Right m3') = compile "f a b c d = a ; g = f 1 2 3 ; t = g 4"
(Right m4) = compile "f x y = x ; g x y = f x y where f x y = y"
(Right m5) = compile "f x y = x ; g x y = f x y"
(Right m6) = compile "f x = g x where g y = reverse y"

(Right m) = compile "f n [] = 0\nf n (_:xs) = n + f n xs\ng = f 2\n"

--------------------------------------------------------------------------------
-- Help Functions
--------------------------------------------------------------------------------
collectNames :: Module -> [String]
collectNames m = nub [ s | Name_Identifier _ _ s <- universeBi m ]

freshVarStrings :: Module -> [String]
freshVarStrings = (['x' : show i | i <- [1..]] \\) . collectNames

allSolutions strat = map (fromContext . snd . last . snd) 
                         (derivations strat $ inContext emptyProg)
normalisedSolutions fs strat = map (normaliseModule fs) $ allSolutions strat
isSolution fs strat m = elem (normaliseModule fs m) $ normalisedSolutions fs strat

checkExercise :: [String] -> Strategy (Context Module) -> [Module] -> (String, String) -> StateT Integer IO ()
checkExercise fs strat solutions (solution, name) = do
    correctCount <- get
    let m = compilation
    let isSolution = m `elem` solutions
    liftIO $ putStrLn $ name ++ " : " ++ show isSolution
    put $ if isSolution then correctCount + 1 else correctCount
  where
    compilation = case compile solution of
                    Right y -> normaliseModule fs y
                    _       -> error $ "no compile: " ++ name

checkExercises :: [String] -> Strategy (Context Module) -> [(String, String)] -> IO ()
checkExercises fs strat es = do
  let solutions = normalisedSolutions fs strat
  (_, count) <- runStateT (mapM (checkExercise fs strat solutions) es) 0
  let percentage = count * 100  `div` toInteger (length es)
  putStrLn $ "\n" ++ take 4 (show percentage) ++ "% has been recognised by the strategy.\n"
  return ()

--------------------------------------------------------------------------------
-- Equality: rewrite to normal form and check syntatically (cannot be solved generally)
--------------------------------------------------------------------------------
equalModules :: [String] -> Module -> Module -> Bool
equalModules fs x y = let nm = normaliseModule fs
                      in nm x == nm y

normaliseModule :: [String] -> Module -> Module
normaliseModule fs = alphaRenaming fs . normalise fs . alphaRenaming fs

normalise :: [String] -> Module -> Module
normalise fs = rewrites . preprocess
  where
    preprocess =  inline fs . anonymise . removeRanges . removeParens
    rewrites = rewriteBi $  (etaReduce 
                        >->  betaReduce 
                        >->  lambdaReduce 
                        >->  applicationReduce 
                        >->  infix2prefix 
                        >->  commutativeOps)

-- Choice do all rewrites in a Module or just one and let it to the rewriteBi
liftRule :: (Data a, Data b) => (a -> Maybe a) -> b -> Maybe b
liftRule rule m =
   safeHead [ fill a | (h, fill) <- contextsBi m, Just a <- [rule h] ]

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
    Expression_NormalApplication r (Expression_Lambda _ [p] e) (a:as) -> do
      e' <- pat2expr p
      return $ Expression_NormalApplication r (subst (e', a) e) as
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


-- Inlining: give a list of desired top level functions, eg. split the body decls in help
-- and main functions. Create a env of to be inlined help functions and remove them from
-- the module. Then do the inlining.

inline :: [String] -> Module -> Module
inline fs m = let (env, m') = putInEnv (map (pat . name) fs) m
              in inlinePatternBindings env m'

putInEnv :: Patterns -> Module -> (Env, Module)
putInEnv ps m = 
  let (decls, replaceDecls) = head (contextsBi m :: [(Declarations, Declarations -> Module)])
      (helpDecls, mainDecls) = partition ((`notElem` ps) . bindingPattern) decls
  in (updateEnv' helpDecls empty, replaceDecls mainDecls)

bindingPattern :: Declaration -> Pattern
bindingPattern d = 
  case d of
    Declaration_PatternBinding _ p _        -> p
    Declaration_FunctionBindings r (fb:fbs) -> pat (funName fb)
    _                                       -> error "not a function!"

{-
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
        reps = [(pat2expr pat, expr) | p <- patterns, 
                                      Declaration_PatternBinding _ pat 
                                        (RightHandSide_Expression _ expr _) <- universeBi p]
    _ -> Nothing

inlinePatBinding :: Declaration -> Maybe Declaration -- from fun binding to pat binding
inlinePatBinding decl =
  case decl of  -- A non recursive function binding, eg. f n = g n ; f 2 = g 3
    Declaration_FunctionBindings _ [fb] -> 
      case fb of
        FunctionBinding_FunctionBinding _ _ (RightHandSide_Expression _ _ MaybeDeclarations_Nothing) ->
          Just $ undefined --anonymise' fb
        _ -> Nothing
    _ -> Nothing
-}

anonymise :: Data a => a -> a
anonymise = transformBi f 
  where 
    f d = 
      case d of
        -- f = 1 : f => f = let f = 1 : f in f 
        Declaration_PatternBinding r p rhs -> 
          if isRecursive d 
          then case pat2expr p of 
                 Just e -> patBinding p (letItBe [d] e) MaybeDeclarations_Nothing
                 _      -> error $ "Prog.hs: pattern " ++ show p ++ " cannot be converted to an expr."
          else d
        -- f n [] = 0; f n (x:xs) = 1 + f xs => f = \n -> let f [] = 0; f (_:xs) = 1 + f xs in f
        Declaration_FunctionBindings r fbs ->
          if isRecursive d then
            -- extract invariant args, use them as pats in a lambda expr and `let' the remaining function
            let args = snd $ unzip $ filter ((==True) . fst) $ invariantArgs d
                -- [(True, [n,m,o]) , ... ] => (\ n -> rename m o to n in expr)
                d' = foldr (\(a:as) -> transformBi (\y -> if y `elem` as then a else y)) d args
                ps = map (expr2pat . head) args

{-
+ detect common arguments
+ rename them
- put in a let and extract them from function calls


f n [] = n
f m (x:xs) = m + f m xs

=>

\ n -> f [] = n
       f (_:xs) = f xs
-}

                (name, _, _, _) = decomposeFB (head fbs)
            -- for efficiency reasons the args of the function calls of name in d should be ps \\ invariantArgs
            in d' --patBinding (pat name) (lambda ps (letItBe [d'] (var name))) 
                 --MaybeDeclarations_Nothing
          else 
            -- just anonymise the function
            let (name, ps, expr, ds) = decomposeFB (head fbs) 
            in patBinding (pat name) (lambda ps expr) ds


functionArgs :: Declaration -> ([Expressions], [Expressions])
functionArgs (Declaration_FunctionBindings _ fbs) = foldr (g . functionArgs') ([],[]) fbs
  where
    g (xs, ys) (xs', ys') = (xs ++ xs', ys ++ ys')

functionArgs' :: FunctionBinding -> ([Expressions], [Expressions])
functionArgs' fb = 
  let (n, ps, _, _) = deconstrucFunBinding fb
      fpats = [map pat2expr ps]
      fargs = [args | Expression_NormalApplication _ 
                        (Expression_Variable _ vn) args <- universeBi fb
                    , n == vn]
  in (fpats, fargs)

-- detect fix ?
isRecursive :: Declaration -> Bool
isRecursive d = 
  case d of 
    Declaration_FunctionBindings _ _   -> (not . null . snd . functionArgs) d
    Declaration_PatternBinding _ p rhs -> pat2expr p `elem` [v | v@(Expression_Variable _ _) <- universeBi rhs]

invariantArgs :: Declaration -> [(Bool, Expressions)]
invariantArgs (Declaration_FunctionBindings _ fbs) = map f $ transpose $ map sameArgs fbs
  where
    f :: [(Bool, Expressions)] -> (Bool, Expressions)
    f xs = let (ys, zs) = unzip xs in (and ys, concat zs) 
    sameArgs fb = let as = allArgs fb in map (\a -> (allSame a, nub a)) as
    allArgs = transpose . uncurry (++) . functionArgs'                
    allSame (x:xs) = all (==x) xs
    allSame [] = False -- will never happen in an functionbinding

-- [ [n,[]] ] T=> [ [n] , [[]] ] => [ (True, [n]) , (True, [[]]) ] 
-- [ [m,(_:xs)] , [m, xs] ] T=> [ [m, m] , [(_:xs), xs] ] => [ (True, [m]) , (False, [(_:xs),xs]) ]

-- [ [ (True, [n]) , (True, [[]]) ] , [ (True, [m]) , (False, [(_:xs),xs]) ] ] T=> [ [(True, [n]) , (True [m]) ] , [(True, [[]]), (F, ...) ] ]
-- => [ (True, [n,m]) , (False, [[], ...]) ]


funName :: FunctionBinding -> Name
funName = (\(n, _, _, _) -> n) . decomposeFB

deconstrucFunBinding :: FunctionBinding -> (Name, Patterns, Expression, MaybeDeclarations)
deconstrucFunBinding (FunctionBinding_FunctionBinding _ lhs rhs) = (name, ps, expr, ds)
  where
    deLHS l = case l of 
                LeftHandSide_Function _ n ps -> (n, ps)
                LeftHandSide_Infix _ l op r   -> (op, [l, r])
                LeftHandSide_Parenthesized _ lhs' ps -> let (n, ps') = deLHS lhs' 
                                                        in (n, ps' ++ ps)
    (name, ps) = deLHS lhs
    (expr, ds) = case rhs of
                    RightHandSide_Expression _ expr w -> (expr, w)
                    RightHandSide_Guarded _ gexpr w   -> error "Todo: guarded expressions"


--------------------------------------------------------------------------------
-- QuickCheck properties
--------------------------------------------------------------------------------

-- Do contract checking for normalised programs