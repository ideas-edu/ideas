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
-- This module contains the domain-specific functions
-----------------------------------------------------------------------------

module Domain.Programming.Prog where

import Common.Utils (safeHead)
import Common.Context
import Common.Derivation
import Common.Strategy hiding (not, repeat, replicate)
import Control.Monad.State
import Data.Generics.Biplate
import Data.Generics.PlateData
import Data.Map (Map, insert, empty, lookup)
import Data.Maybe
import Data.Data hiding (Fixity)
import Data.List hiding (union, lookup, insert)
import Domain.Programming.AlphaRenaming (alphaRenaming)
import Domain.Programming.Anonymise
import Domain.Programming.Helium
import Domain.Programming.HeliumRules
import Domain.Programming.InlinePatternBindings (Env, updateEnv'
                                                , inlinePatternBindings)
import Domain.Programming.Substitute
import Domain.Programming.Utils
import Prelude hiding (lookup)


-- Test values
(Right m') = compile "f x y = x + y"
(Right m2) = compile "f = \\ y -> \\x -> div x y"
(Right m3) = compile "f a b c d = a ; g = ((f 1) 2 3) ; h = g 4"
(Right m3') = compile "f a b c d = a ; g = f 1 2 3 ; t = g 4"
(Right m4) = compile "f x y = x ; g x y = f x y where f x y = y"
(Right m5) = compile "f x y = x ; g x y = f x y"
(Right m6) = compile "f x = g x where g y = reverse y"
(Right m) = compile "f n [] = 0\nf m (_:xs) = m + f m xs\ng = f 2\n"
checkFromBin = checkExercises ["fromBin"]


--------------------------------------------------------------------------------
-- Data types
--------------------------------------------------------------------------------
data Solution = Solution { solution :: String
                         , sid      :: String
                         , strat    :: LabeledStrategy (Context Module)
                         , remark   :: String }

type Solutions = [Solution]


--------------------------------------------------------------------------------
-- Help Functions
--------------------------------------------------------------------------------
collectNames :: Module -> [String]
collectNames m = nub [ s | Name_Identifier _ _ s <- universeBi m ]

freshVarStrings :: Module -> [String]
freshVarStrings = (['x' : show i | i <- [1..]] \\) . collectNames
allSolutions strategy = map fromContext $ results $ derivationTree strategy $ inContext emptyProg
normalisedSolutions fs = map (normaliseModule fs) . allSolutions
isSolution fs strat m = elem (normaliseModule fs m) $ normalisedSolutions fs strat

checkExercise :: [String] -> Solution -> StateT Integer IO ()
checkExercise fixedNames s = do
    correctCount <- get
    let m = compilation
    let isSolution = m `elem` normalisedSolutions fixedNames (toStrategy (strat s))
    liftIO $ do putStrLn $ pps (sid s) ++ pps (show isSolution)
                        ++ pps (strategyName (strat s)) ++ ppStringS 40 (remark s)
                putStrLn line
    put $ if isSolution then correctCount + 1 else correctCount
  where
    compilation = case compile (solution s) of
                    Right y -> normaliseModule fixedNames y
                    _       -> error $ "no compile: " ++ sid s
    pps = ppStringS 20

checkExercises :: [String] -> Solutions -> IO ()
checkExercises fixedNames solutions = do
  report (null fixedNames) "At least one function name should be given"
  let pps = ppStringS 20
  putStrLn $ line ++ "\n" ++ pps "Identifier" ++ pps "Is recognised" 
          ++ pps "By strategy" ++ ppStringS 40 "Remark" ++ "\n" ++ dline
  (_, count) <- runStateT (mapM (checkExercise fixedNames) solutions) 0
  let percentage = take 4 $ show $ count * 100  `div` toInteger (length solutions)
  putStrLn $ "\n" ++ percentage ++ "% has been recognised by the strategy.\n"
  return ()

ppStringS i s = "| " ++ s ++ replicate (i - length s) ' '
line = replicate 80 '-'
dline = replicate 80 '='
report b s = if b then error s
             else return ()

--------------------------------------------------------------------------------
-- Equality: rewrite to normal form and check syntatically (cannot be solved generally)
--------------------------------------------------------------------------------
equalModules :: [String] -> Module -> Module -> Bool
equalModules fs x y = let nm = normaliseModule fs
                      in nm x == nm y

normaliseModule :: [String] -> Module -> Module
normaliseModule fs = alphaRenaming [] . normalise ns . alphaRenaming ns
  where 
    ns = map name fs

normalise :: Names -> Module -> Module
normalise fs = rewrites . preprocess
  where
    preprocess =  inline fs . makeBetaReducible fs . removeExplicitApps . removeRanges . removeParens
    rewrites = rewriteBi $  (applicationReduce
                        >->  betaReduce 
                        >->  lambdaReduce 
                        >->  etaReduce
                        >->  infix2prefix 
                        >->  commutativeOps)
                       >>->  cleanUpLet

-- Choice do all rewrites in a Module or just one and let it to the rewriteBi
liftRule :: (Data a, Data b) => (a -> Maybe a) -> b -> Maybe b
liftRule rule m =
   safeHead [ fill a | (h, fill) <- contextsBi m, Just a <- [rule h] ]

(>->) :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
(f >-> g) x = f x `mplus` g x
infixr 1 >->

(>>->) :: (Data a, Data b) => (a -> Maybe a) -> (b -> Maybe b) -> Module -> Maybe Module
(f >>-> g) x = liftRule f x `mplus` liftRule g x
infixr 2 >>->

removeParens :: Data a => a -> a
removeParens = transformBi f . transformBi g
  where f (Expression_Parenthesized _ expr) = expr
        f x = x
        g (Pattern_Parenthesized _ pat) = pat
        g x = x

removeExplicitApps :: Data a => a -> a -- could also be done in strategy, the easy way out for now
removeExplicitApps = transformBi f
  where 
    f (Expression_InfixApplication r
        (MaybeExpression_Just f) 
        (Expression_Variable _ (Name_Operator _ _ "$")) 
        (MaybeExpression_Just arg)) = Expression_NormalApplication r f [arg]
    f x = x

removeRanges :: Data a => a -> a
removeRanges = transformBi (\(Range_Range  _ _) -> noRange)


-- eta reduction, e.g. \x -> f x => f
etaReduce :: Expression -> Maybe Expression
etaReduce expr = 
  case expr of 
    Expression_Lambda _ [Pattern_Variable _ p] 
      (Expression_NormalApplication r f args@(_:_)) -> 
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
    Expression_NormalApplication _ f [] -> Just f
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
        guard (not (isSorted args))
        guard (isOp f "+" || isOp f "*") -- should be done elsewhere, in preludeS?
        return $ Expression_NormalApplication r
                   (Expression_InfixApplication r' MaybeExpression_Nothing 
                                                f MaybeExpression_Nothing) $ sort args
    _ -> Nothing
  where
    isSorted xs = sort xs == xs
    isOp op n = case op of
                  Expression_Variable _ (Name_Operator _ [] n') -> n == n'
                  _ -> False

cleanUpLet :: Declaration -> Maybe Declaration
cleanUpLet x = 
  case x of
    Declaration_PatternBinding _ _ 
      (RightHandSide_Expression _ (Expression_Let _ [decl] (Expression_Variable _ _)) 
        MaybeDeclarations_Nothing) -> Just decl
    _ -> Nothing


--------------------------------------------------------------------------------
-- Inlining
--------------------------------------------------------------------------------
inline :: Names -> Module -> Module
inline fs = go . go
  where 
    go = uncurry inlinePatternBindings . putInEnv (map pat fs)

putInEnv :: Patterns -> Module -> (Env, Module)
putInEnv ps m = 
  let (decls, replaceDecls) = head (contextsBi m :: [(Declarations, Declarations -> Module)])
      (helpDecls, mainDecls) = partition ((`notElem` ps) . bindingPattern) decls
  in if null mainDecls then error "Please give at least one main function!"
     else (updateEnv' helpDecls empty, replaceDecls mainDecls)

bindingPattern :: Declaration -> Pattern
bindingPattern d = 
  case d of
    Declaration_PatternBinding _ p _        -> p
    Declaration_FunctionBindings r (fb:fbs) -> pat (funName fb)
    _                                       -> error "not a function!"

--------------------------------------------------------------------------------
-- QuickCheck properties
--------------------------------------------------------------------------------

-- Do contract checking for normalised programs