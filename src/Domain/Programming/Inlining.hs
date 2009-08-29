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
-- Inlining: give a list of desired top level functions, eg. split the body 
-- decls in help and main functions. Create a env of to be inlined help 
-- functions and remove them from the module. Then do the inlining.
--
-----------------------------------------------------------------------------

module Domain.Programming.Inlining where

import Data.Data hiding (Fixity)
import Data.Generics.Biplate
import Data.Generics.PlateData
import Data.List (partition, sort, transpose, (\\))
import Data.Map (Map, insert, empty, lookup)
import Data.Maybe
import Domain.Programming.Helium
import Domain.Programming.HeliumRules
import Domain.Programming.InlinePatternBindings (Env, inlinePatternBindings, updateEnv')
import Domain.Programming.Utils

inline :: [String] -> Module -> Module
inline fs m = let (env, m') = putInEnv (map (pat . name) fs) m
              in inlinePatternBindings env m'

--------------------------------------------------------------------------------
-- Help Functions
--------------------------------------------------------------------------------

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

   
--------------------------------------------------------------------------------
-- Help Functions
--------------------------------------------------------------------------------
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
            let args = map (reverse . sort) $ catMaybes $ invariantArgs fbs :: [Expressions]               
                d' = foldr (\(a:as) -> transformBi (\y -> if y `elem` as then a else y)) d args          
                ps = map (fromMaybe (error ("Prog.hs: no conversion from expr to pat: " ++ show args)) . expr2pat . head) args
                d'' = transformBi (removeInvArgs ps (map head args)) d'
                (name, _, _, _) = deconstrucFunBinding (head fbs)
            -- for efficiency reasons the args of the function calls of name in d should be ps \\ invariantArgs
            in if not (null ps) 
               then patBinding (pat name) (lambda ps (letItBe [d''] (var name))) MaybeDeclarations_Nothing
               else patBinding (pat name) (letItBe [d'] (var name)) MaybeDeclarations_Nothing
         else 
            -- just anonymise the function
            let (name, ps, expr, ds) = deconstrucFunBinding (head fbs) 
            in patBinding (pat name) (lambda ps expr) ds
        x -> x

removeInvArgs :: Patterns -> Expressions -> FunctionBinding -> FunctionBinding
removeInvArgs invPs invArgs = transformBi remPats . transformBi remArgs
  where
    remPats x = case x of
                  (ps :: Patterns) -> (ps \\ invPs) \\ [Pattern_Wildcard noRange]
                  _                -> x
    remArgs x = case x of 
                  Expression_NormalApplication r f args -> 
                    Expression_NormalApplication r f (args \\ invArgs)
                  _ -> x
{- todo:          Expression_InfixApplication r' l op r ->
                    Expression_InfixApplication r' (if l `elem` invArgs 
                                                    then MaybeExpression_Nothing
                                                    else l)
                                                    op 
                                                    (if r `elem` invArgs 
                                                     then MaybeExpression_Nothing
                                                     else r)
-}

functionArgs :: FunctionBinding -> [[Maybe Expression]]
functionArgs fb = 
  let (n, ps, _, _) = deconstrucFunBinding fb
      fpats = map pat2expr ps
      fargs = [map Just args | Expression_NormalApplication _ 
                                 (Expression_Variable _ vn) args <- universeBi fb
                             , n == vn]
  in fpats : fargs

-- detect fix ?
isRecursive :: Declaration -> Bool
isRecursive d = 
  case d of 
    Declaration_FunctionBindings _ fbs -> any (== var (funName (head fbs))) $ universeBi fbs 
    Declaration_PatternBinding _ p rhs -> 
      case pat2expr p of
        Just e -> e `elem` [v | v@(Expression_Variable _ _) <- universeBi rhs]
        _      -> error "Prog.hs: pattern not convertible to expr."

invariantArgs :: FunctionBindings -> [Maybe Expressions]
invariantArgs = (map (foldr mulMaybe (Just []))) . transpose . (map sameArgs)
    where
      sameArgs = (map sameMaybe) . transpose . functionArgs
      mulMaybe (Just x) (Just ys) = Just (x:ys)
      mulMaybe _        _         = Nothing
      sameMaybe = foldr1 f
        where f x y = case (x,y) of
                        (Just x', Just y') -> if x' == y' || x' == wildcard || y' == wildcard
                                              then x else Nothing
                        _                  -> Nothing
              wildcard = var $ name "_"

