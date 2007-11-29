{--------------------------------------------------- 
Copyright (c)        2006 - 2007 
Johan Jeuring and Harrie Passier
---------------------------------------------------}

module Logic.Solver.LogicPretty (prettyLogic, form2string) where

-- GHC library
import GHC.Real

-- UU libraries
import UU.Pretty 

import Logic.Domain

instance PP Integer where
  pp i  =  text (show i)


{- testje :: Logic -> Bool
testje p = trace (s1 ++ "  ;   " ++ s2) (s1==s2)
 where s1 = ppLogic p 
       s2 = disp (prettyLogic p) 1000 "" -}

prettyLogic :: Logic -> PP_Doc
prettyLogic = prettyLogic' 0
 
test1 = prettyLogic (Not (Not (Var "p" :&&: Var "q")))
 
pp_parens_if c pp = if c then pp_parens pp else pp


-- Nog aanpassen
form2string       formula       = disp (prettyLogic formula) 40 "" -- replaceBrackets (removeBrackets (disp (prettyLogic formula) 40 ""))

-- Tijdelijke functie
removeBrackets :: String -> String
removeBrackets []       = []
removeBrackets ('(':xs) = removeBrackets xs
removeBrackets (')':xs) = removeBrackets xs
removeBrackets (x:xs)   = x:removeBrackets xs 

-- Tijdelijke functie
replaceBrackets :: String -> String
replaceBrackets []       = []
replaceBrackets ('!':xs) = '(' : replaceBrackets xs
replaceBrackets ('%':xs) = ')' : replaceBrackets xs
replaceBrackets (x:xs)   = x : replaceBrackets xs 


prettyLogic'  :: Int -> Logic -> PP_Doc
prettyLogic'  =  \p e -> case e of
  Var v        ->  pp v
  f1 :->:  f2  ->  pp_parens_if (p>3) (prettyLogic' 4 f1 >#< "->"  >#< prettyLogic' 4 f2)
  f1 :<->: f2  ->  pp_parens_if (p>3) (prettyLogic' 4 f1 >#< "<->" >#< prettyLogic' 4 f2)
  f1 :||:  f2  ->  pp_parens_if (p>3) (prettyLogic' 4 f1 >#< "||"  >#< prettyLogic' 4 f2)
  f1 :&&:  f2  ->  pp_parens_if (p>3) (prettyLogic' 4 f1 >#< "/\\" >#< prettyLogic' 4 f2)
  Not (Not f)  ->  pp ("~~") >|< prettyLogic' 8 f
  Not f        ->  pp ("~") >|< prettyLogic' 8 f
  T            ->  pp ('T')
  F            ->  pp ('F')


{- 
prettyLogic'  :: Int -> Logic -> PP_Doc
prettyLogic'  =  \p e -> case e of
  Var v        ->  pp v
  f1 :->:  f2  ->  pp_parens_if (p>4) (prettyLogic' 4 f1 >#< "->"  >#< prettyLogic' 4 f2)
  f1 :<->: f2  ->  pp_parens_if (p>5) (prettyLogic' 5 f1 >#< "<->" >#< prettyLogic' 5 f2)
  f1 :||:  f2  ->  pp_parens_if (p>6) (prettyLogic' 6 f1 >#< "||"  >#< prettyLogic' 6 f2)
  f1 :&&:  f2  ->  pp_parens_if (p>7) (prettyLogic' 7 f1 >#< "/\\" >#< prettyLogic' 7 f2)
  Not f        ->  pp ("~") >|< prettyLogic' 8 f
  T            ->  pp ('T')
  F            ->  pp ('F')

-}
{-


-- First level: no (outside) brackets are needed. 
prettyLogic'  :: Int -> Logic -> PP_Doc
prettyLogic'  =  \p e -> case e of
  Var v        ->  pp v
  f1 :->:  f2  ->  pp_parens_if (p>4) (pp (prettyLogic'' 4 f1 >#< "->"  >#< prettyLogic'' 4 f2))
  f1 :<->: f2  ->  pp_parens_if (p>5) (pp (prettyLogic'' 5 f1 >#< "<->" >#< prettyLogic'' 5 f2))
  f1 :&&:  f2  ->  pp_parens_if (p>6) (pp (prettyLogic'' 6 f1 >#< "/\\" >#< prettyLogic'' 6 f2))
  f1 :||:  f2  ->  pp_parens_if (p>7) (pp (prettyLogic'' 7 f1 >#< "||"  >#< prettyLogic'' 7 f2))
  Not f        ->  pp ("~") >|< prettyLogic'' 7 f
  T            ->  pp ('T')
  F            ->  pp ('F')


-- Second and next levels: brackets are needed.
prettyLogic''  :: Int -> Logic -> PP_Doc
prettyLogic''  =  \p e -> case e of
  Var v        ->  pp v
  f1 :->:  f2  ->  pp_parens_if (p>4) (pp ('!') >|< prettyLogic'' 4 f1 >#< "->"  >#< prettyLogic'' 4 f2 >|< pp ('%'))
  f1 :<->: f2  ->  pp_parens_if (p>5) (pp ('!') >|< prettyLogic'' 4 f1 >#< "<->" >#< prettyLogic'' 5 f2 >|< pp ('%'))
  f1 :&&:  f2  ->  pp_parens_if (p>6) (pp ('!') >|< prettyLogic'' 6 f1 >#< "/\\" >#< prettyLogic'' 6 f2 >|< pp ('%'))
  f1 :||:  f2  ->  pp_parens_if (p>7) (pp ('!') >|< prettyLogic'' 7 f1 >#< "||"  >#< prettyLogic'' 7 f2 >|< pp ('%'))
  Not f        ->  pp ("~") >|< prettyLogic'' 7 f
  T            ->  pp ('T')
  F            ->  pp ('F')
-}