{--------------------------------------------------- 
Copyright (c)        2006 - 2007 
Johan Jeuring and Harrie Passier
---------------------------------------------------}

module LogicPretty where

-- GHC library
import GHC.Real

-- UU libraries
import UU.Pretty 

-- Equations model
import LogicFormula 

instance PP Integer where
  pp i  =  text (show i)


prettyFormula :: Formula -> PP_Doc
prettyFormula = prettyFormula' 0
 
pp_parens_if c pp = if c then pp_parens pp else pp


-- Nog aanpassen
form2string       formula       = disp (prettyFormula formula) 40 "" -- replaceBrackets (removeBrackets (disp (prettyFormula formula) 40 ""))

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


prettyFormula'  :: Int -> Formula -> PP_Doc
prettyFormula'  =  \p e -> case e of
  Var v        ->  pp v
  f1 :->:  f2  ->  pp_parens_if (p>3) (prettyFormula' 4 f1 >#< "->"  >#< prettyFormula' 4 f2)
  f1 :<->: f2  ->  pp_parens_if (p>3) (prettyFormula' 4 f1 >#< "<->" >#< prettyFormula' 4 f2)
  f1 :||:  f2  ->  pp_parens_if (p>3) (prettyFormula' 4 f1 >#< "||"  >#< prettyFormula' 4 f2)
  f1 :&&:  f2  ->  pp_parens_if (p>3) (prettyFormula' 4 f1 >#< "/\\" >#< prettyFormula' 4 f2)
  Not f        ->  pp ("~") >|< prettyFormula' 8 f
  T            ->  pp ('T')
  F            ->  pp ('F')


{- 
prettyFormula'  :: Int -> Formula -> PP_Doc
prettyFormula'  =  \p e -> case e of
  Var v        ->  pp v
  f1 :->:  f2  ->  pp_parens_if (p>4) (prettyFormula' 4 f1 >#< "->"  >#< prettyFormula' 4 f2)
  f1 :<->: f2  ->  pp_parens_if (p>5) (prettyFormula' 5 f1 >#< "<->" >#< prettyFormula' 5 f2)
  f1 :||:  f2  ->  pp_parens_if (p>6) (prettyFormula' 6 f1 >#< "||"  >#< prettyFormula' 6 f2)
  f1 :&&:  f2  ->  pp_parens_if (p>7) (prettyFormula' 7 f1 >#< "/\\" >#< prettyFormula' 7 f2)
  Not f        ->  pp ("~") >|< prettyFormula' 8 f
  T            ->  pp ('T')
  F            ->  pp ('F')

-}
{-


-- First level: no (outside) brackets are needed. 
prettyFormula'  :: Int -> Formula -> PP_Doc
prettyFormula'  =  \p e -> case e of
  Var v        ->  pp v
  f1 :->:  f2  ->  pp_parens_if (p>4) (pp (prettyFormula'' 4 f1 >#< "->"  >#< prettyFormula'' 4 f2))
  f1 :<->: f2  ->  pp_parens_if (p>5) (pp (prettyFormula'' 5 f1 >#< "<->" >#< prettyFormula'' 5 f2))
  f1 :&&:  f2  ->  pp_parens_if (p>6) (pp (prettyFormula'' 6 f1 >#< "/\\" >#< prettyFormula'' 6 f2))
  f1 :||:  f2  ->  pp_parens_if (p>7) (pp (prettyFormula'' 7 f1 >#< "||"  >#< prettyFormula'' 7 f2))
  Not f        ->  pp ("~") >|< prettyFormula'' 7 f
  T            ->  pp ('T')
  F            ->  pp ('F')


-- Second and next levels: brackets are needed.
prettyFormula''  :: Int -> Formula -> PP_Doc
prettyFormula''  =  \p e -> case e of
  Var v        ->  pp v
  f1 :->:  f2  ->  pp_parens_if (p>4) (pp ('!') >|< prettyFormula'' 4 f1 >#< "->"  >#< prettyFormula'' 4 f2 >|< pp ('%'))
  f1 :<->: f2  ->  pp_parens_if (p>5) (pp ('!') >|< prettyFormula'' 4 f1 >#< "<->" >#< prettyFormula'' 5 f2 >|< pp ('%'))
  f1 :&&:  f2  ->  pp_parens_if (p>6) (pp ('!') >|< prettyFormula'' 6 f1 >#< "/\\" >#< prettyFormula'' 6 f2 >|< pp ('%'))
  f1 :||:  f2  ->  pp_parens_if (p>7) (pp ('!') >|< prettyFormula'' 7 f1 >#< "||"  >#< prettyFormula'' 7 f2 >|< pp ('%'))
  Not f        ->  pp ("~") >|< prettyFormula'' 7 f
  T            ->  pp ('T')
  F            ->  pp ('F')
-}