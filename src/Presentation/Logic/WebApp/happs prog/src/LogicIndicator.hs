{---------------------------------------------------------------
 Copyright (c)        2006 - 2007 
 Johan Jeuring and Harrie Passier
----------------------------------------------------------------}

--

module LogicIndicator where


-- Standard Haskell libraries
import List
import Maybe
import LogicFormula

data Indicators = Ind { numOfImp       :: Int  -- The number of implications
                      , numOfEqv       :: Int  -- The number of equivalences
                      , numOfDoubleNeg :: Int  -- The number of double negations
                      }           
                      
                      deriving (Show, Eq)



indicators :: Formula  -> Indicators 
indicators formula
                =  Ind { numOfImp       = numberOfImpl      formula
                       , numOfEqv       = numberOfEqv       formula
                       , numOfDoubleNeg = numberOfDoubleNeg formula
                       }  

-- Count the number of implications in formula f
numberOfImpl:: Formula -> Int
numberOfImpl f =  
  case f of
     Var v         ->  0
     (f1 :->: f2)  ->  1 + numberOfImpl f1 + numberOfImpl f2 
     (f1 :<->: f2) ->      numberOfImpl f1 + numberOfImpl f2 
     (f1 :&&: f2)  ->      numberOfImpl f1 + numberOfImpl f2 
     (f1 :||: f2)  ->      numberOfImpl f1 + numberOfImpl f2 
     Not f         ->      numberOfImpl f
     T             ->  0
     F             ->  0 


-- Count the number of equivalences in formula f
numberOfEqv :: Formula -> Int
numberOfEqv f =  
  case f of
     Var v         ->  0
     (f1 :->: f2)  ->     numberOfEqv f1 + numberOfEqv f2 
     (f1 :<->: f2) ->  1+ numberOfEqv f1 + numberOfEqv f2 
     (f1 :&&: f2)  ->     numberOfEqv f1 + numberOfEqv f2 
     (f1 :||: f2)  ->     numberOfEqv f1 + numberOfEqv f2 
     Not f         ->     numberOfEqv f
     T             ->  0
     F             ->  0 


-- Count the number of double negations in formula f
numberOfDoubleNeg :: Formula -> Int
numberOfDoubleNeg f =  
  case f of
     Var v         ->  0
     (f1 :->: f2)  ->      numberOfDoubleNeg f1 + numberOfDoubleNeg f2 
     (f1 :<->: f2) ->      numberOfDoubleNeg f1 + numberOfDoubleNeg f2 
     (f1 :&&: f2)  ->      numberOfDoubleNeg f1 + numberOfDoubleNeg f2 
     (f1 :||: f2)  ->      numberOfDoubleNeg f1 + numberOfDoubleNeg f2 
     Not (Not f)   ->  1 + numberOfDoubleNeg f
     Not f         ->      numberOfDoubleNeg f
     T             ->  0
     F             ->  0 

                                      


