{-# OPTIONS -XScopedTypeVariables #-}

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

module Domain.Programming.Anonymise where

import Control.Monad
import Data.Data hiding (Fixity)
import Data.Generics.Biplate
import Data.Generics.PlateData
import Data.List (sort, transpose, (\\))
import Data.Maybe
import Domain.Programming.AlphaRenaming (AlphaRenaming, alphaRenaming)
import Domain.Programming.Helium
import Domain.Programming.HeliumRules
import Domain.Programming.Utils


makeBetaReducible :: (AlphaRenaming a, Data a) => Names -> a -> a
makeBetaReducible fs = where2let . letRecursivePatDecl . anonymise' . alphaRenaming fs . optimise

optimise :: Data a => a -> a
optimise = transformBi optimiseFunBindings
  where
    optimiseFunBindings d@(Declaration_FunctionBindings r fbs) =
      if isRecursive d then
        let invArgsLists = map (reverse . sort) $ catMaybes $ invariantArgs fbs
            fname = funName $ head fbs
            invArgs = map head $ invArgsLists               
        in if not (null invArgs) then
             let d' = foldr (\(a:as) -> transformBi (\y -> if y `elem` as then a else y)) 
                            d invArgsLists
                 invPs = let err = error "Anonymise.hs: no conversion from expr to pat."
                         in map (fromMaybe err . expr2pat) invArgs
             in declFunBindings 
                  [funBinding fname invPs
                              (letItBe [removeArgs invArgs d'] (var fname))
                  ]                          
           else    
             d
      else
        d
    optimiseFunBindings x = x

anonymise' :: Data a => a -> a
anonymise' = transformBi anonymiseFunBinding 
  where 
    anonymiseFunBinding d@(Declaration_FunctionBindings _ [fb]) =
      if not (isRecursive d) then
        let (name, ps, expr, w) = deconstructFunBinding fb
        in patBinding (pat name) (lambda ps expr) w
      else d
    anonymiseFunBinding x = x

letRecursivePatDecl :: Data a => a -> a
letRecursivePatDecl = transformBi toLet
  where
    toLet d@(Declaration_PatternBinding _ p rhs) =
      if isRecursive d then 
        maybe d (\e -> patBinding p (letItBe [d] e) MaybeDeclarations_Nothing) $ pat2expr p
      else
        d
    toLet x = x

where2let :: Data a => a -> a
where2let = transformBi toLet
  where
    toLet rhs@(RightHandSide_Expression r expr w) = 
      case fromMaybeDecls w of
        Just ds -> RightHandSide_Expression r (letItBe ds expr) MaybeDeclarations_Nothing
        _ -> rhs
    toLet x = x

{- 
cases to consider:

- pattern bindings -> if recursive then place in a let else nothing
- function bindings ->
       1. first optimise functions:
             eg. f n []     = n
                 f n (x:xs) = x*n + f n xs
             =>
                 f n = let f' []     = n
                           f' (x:xs) = x*n + f' xs  !! alpha rename

       2. anonymise functions (ie. make them beta reducible):
            if not recursive and one functionbinding -> lambdaise :
              eg. f n = n + 1 => f = \n -> n + 1

       3. rewrite recursive pattern declarations: 
              eg. d = 1 : d => d = let d' = 1 : d'
                                   in d'     



                   eg. f [] = 0
                       f (x:xs) = x  !! our beta reduction does not yet go this far
                   =>
                       f = \x -> case x of
                                   [] -> 0
                                   (x:xs) -> x
                                   


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
            let args = map (reverse . sort) $ catMaybes $ invariantArgs fbs :: [Expressions]               
                d' = foldr (\(a:as) -> transformBi (\y -> if y `elem` as then a else y)) d args          
                ps = map (fromMaybe (error ("Prog.hs: no conversion from expr to pat: " ++ show args)) . expr2pat . head) args
                d'' = transformBi (removeArgs (map head args)) d'
                name = funName (head fbs)
            in if not (null ps) 
               then patBinding (pat name) (lambda ps (letItBe [d''] (var name))) MaybeDeclarations_Nothing
               else patBinding (pat name) (letItBe [d'] (var name)) MaybeDeclarations_Nothing
         else 
            -- just anonymise the function
            let (name, ps, expr, ds) = deconstructFunBinding (head fbs) 
            in patBinding (pat name) (lambda ps expr) ds
        x -> x

removeArgs :: Expressions -> Declaration -> Declaration
removeArgs invArgs d =
  case d of 
    Declaration_FunctionBindings _ fbs -> 
      transformBi remPats $ transformBi remArgs d
        where
          fname = var $ funName $ head fbs
          invPs = let err = error "Anonymise.hs: no conversion from expr to pat."
                  in map (fromMaybe err . expr2pat) invArgs
          remPats x = 
            case x of
              (ps :: Patterns) -> (ps \\ invPs) \\ [Pattern_Wildcard noRange]
              _                -> x
          remArgs x = 
            case x of 
              Expression_NormalApplication r f args -> 
                if fname == f 
                then Expression_NormalApplication r f (args \\ invArgs)
                else x
              _ -> x
    _ -> d

{- todo:          Expression_InfixApplication r' l op r ->
                    Expression_InfixApplication r' (if l `elem` invArgs 
                                                    then MaybeExpression_Nothing
                                                    else l)
                                                    op 
                                                    (if r `elem` invArgs 
                                                     then MaybeExpression_Nothing
                                                     else r)
-}

funArgs :: FunctionBinding -> [[Maybe Expression]]
funArgs fb = 
  let (n, ps, _, _) = deconstructFunBinding fb
      fpats = map pat2expr ps
      fargs = [map Just args | Expression_NormalApplication _ 
                                 (Expression_Variable _ vn) args <- universeBi fb
                             , n == vn]
  in fpats : fargs

-- detect fix ?
isRecursive :: Declaration -> Bool
isRecursive d = 
  case d of 
    Declaration_FunctionBindings _ fbs -> 
      any (== var (funName (head fbs))) $ universeBi fbs 
    Declaration_PatternBinding _ p rhs -> 
      maybe False (`elem` [v | v@(Expression_Variable _ _) <- universeBi rhs]) $ pat2expr p
    _ -> error "Anonymise.hs: not a function declration."

invariantArgs :: FunctionBindings -> [Maybe Expressions]
invariantArgs = map (foldr mulMaybe (Just [])) . transpose . map eqArgs
    where
      eqArgs = map eqMaybes . transpose . funArgs
      mulMaybe m1 m2 = do
        x <- m1; ys <- m2
        return (x:ys)
      eqMaybes = foldr1 eqMaybe
        where
          wildcard = var $ name "_"
          eqMaybe m1 m2 = do
            x <- m1 ; y <- m2
            guard $ x == y || x == wildcard || y == wildcard
            return x


