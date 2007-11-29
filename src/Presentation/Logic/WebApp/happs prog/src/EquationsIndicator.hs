{---------------------------------------------------------------
 Copyright (c)        2006 - 2007 
 Johan Jeuring and Harrie Passier
----------------------------------------------------------------}


module EquationsIndicator where


-- Standard Haskell libraries
import List
import Maybe


-- Equations models
-- import EquationsUtility

import Equations           ( 
                             Equations, Equation(..), Expr(..), VarList, isSolved, leftExpr,
                             sizeExpr, varsEquation, varsEquations
                           )
import EquationsUtility    ( fst3 )   


data Indicators = Ind { numOfVarsSolved         :: Int
                      , numofOccurrencesOfVars  :: Int
                      , sizeOfLeftHandSideExprs :: Int
                      }           
                      
                      deriving (Show, Eq)


indicators :: Equations -> Indicators 
indicators equations
                =  Ind { numOfVarsSolved         = fst3 (varsSolved equations)
                       , numofOccurrencesOfVars  = sum . map (length . nub . varsEquation) $ equations
                       , sizeOfLeftHandSideExprs = sum (map (sizeExpr . leftExpr) equations)
                       }   
                                         

{----------------------------------------------------------------------
Indicator 'varsSolved' determines the number (Int) of variables for which
a solution is found. Variable x is solved if there exists an equation of
the form of x = c. The solved variables are listed in a list [Var].
-----------------------------------------------------------------------}


varsSolved      :: Equations -> (Int, VarList, VarList)
varsSolved sle = 
     let varsSolved    = (nub . varsEquations) (filter isSolved sle) 
         numVarsSolved = length varsSolved  
         allVars       = nub (varsEquations sle)
     in (numVarsSolved, varsSolved, allVars) 



 
{----------------------------------------------------------------------
Indicator 'varsDecreased' determines if the number of occurrences of
variables in a system of linear equations decreases. This number often
decreases after a substitution. Note that there is one occurrence of 
variable x in the system "2*x+x=3". 
-----------------------------------------------------------------------}


varsDecreased :: Equations -> Equations -> (Bool, Int , Int) 
varsDecreased sle1 sle2 =
        let numOfVarsS1 = numOfVars sle1
            numOfVarsS2 = numOfVars sle2
        in (numOfVarsS2 < numOfVarsS1, numOfVarsS1, numOfVarsS2)   
  where numOfVars  :: Equations -> Int
        numOfVars  =  sum . map (length . nub . varsEquation) 


{-----------------------------------------------------------------------
Indicator 'lhsDecreased' checks for all equations if the expression size
of the left hand side has decreased. Futhermore, the positions of the
equations of which the left hand side is decreased are listed.  
sle1 precedes sle2
------------------------------------------------------------------------} 


lhsDecreased :: Equations -> Equations -> (Bool,[Int])
lhsDecreased sle1 sle2 =
  let sizes1         = map (\ (l :=: r) -> sizeExpr l) sle1      
      sizes2         = map (\ (l :=: r) -> sizeExpr l) sle2
      difflhs        = map (\(x,y) -> x > y) (zip sizes1 sizes2)
      lhssdecreased  = or difflhs
      numOfEquations = findIndices (==True) difflhs
  in (lhssdecreased, numOfEquations)

