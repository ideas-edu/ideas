{--------------------------------------------------- 
Copyright (c)        2005 - 2006 
Johan Jeuring and Harrie Passier
---------------------------------------------------}

module EquationsPretty where

-- GHC library
import GHC.Real

-- UU libraries
import UU.Pretty 

-- Equations model
import Equations (Equations(..), Equation(..), Expr(..))

instance PP Integer where
  pp i  =  text (show i)
  
instance (PP a,Integral a) => PP (Ratio a) where
  pp (a :% b)  =  if b==1 then pp a else pp a >|< "/" >|< pp b

prettyEquations  :: Equations -> PP_Doc
prettyEquations  =  foldr (>-<) empty . map prettyEquation  

prettyEquation            :: Equation -> PP_Doc
prettyEquation (l :=: r)  =  prettyExpr 0 l >#< "=" >#< prettyExpr 0 r
 
pp_parens_if c pp = if c then pp_parens pp else pp

prettyExpr  :: Int -> Expr -> PP_Doc
prettyExpr  =  \p e -> case e of
  l :+: r  ->  pp_parens_if (p>4) (prettyExpr 4 l >|< "+" >|< prettyExpr 4 r)
  l :-: r  ->  pp_parens_if (p>5) (prettyExpr 4 l >|< "-" >|< prettyExpr 5 r)
  l :*: r  ->  if l==Con (1%1) 
               then prettyExpr p r 
               else if r==Con (1%1)
                    then prettyExpr p l
                    else pp_parens_if (p>6) (prettyExpr 6 l >|< "*" >|< prettyExpr 6 r)
  l :/: r  ->  pp_parens_if (p>7) (prettyExpr 7 l >|< "/" >|< prettyExpr 7 r)
  Con r    ->  pp r
  Var s    ->  pp s
  Zero     ->  pp (0::Int)

equations2string  equations  =  disp (prettyEquations equations) 40 ""
equation2string   equation   =  disp (prettyEquation  equation ) 40 ""
expr2string       expr       =  disp (prettyExpr 0    expr     ) 40 ""

-- test = expr2string (Con (2:%5) :-: Var "x")



-- prettySolution :: Solution -> String
prettySolution =  foldr1 (>-<)  . map (\(x,y) -> text (x ++ ":") >#< prettyExpr 0 y)

