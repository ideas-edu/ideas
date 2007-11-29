{------------------------------------------------------------------ 
Copyright (c)        2006 - 2007 
Johan Jeuring and Harrie Passier
------------------------------------------------------------------}


module Logic.Solver.LogicGenerator where

import Logic
import Logic.Solver.LogicIndicator
import Control.Monad
import Test.QuickCheck hiding (defaultConfig)
import System.Random

{- !!! TODO: move this outside the logic solver !!! -}

testje = suitableFormula (mkStdGen 28)

suitableFormula :: StdGen -> Logic
suitableFormula = suitableFormulaWith defaultConfig
            
suitableFormulaWith :: LogicGenConfig -> StdGen -> Logic
suitableFormulaWith config stdGen
   | suitable  = logic
   | otherwise = suitableFormulaWith config (fst $ split stdGen)
 where
   logic    = generateLogic stdGen
   suitable =  not (isDNF logic)
            && countEquivalences logic < 2
            && countBinaryOperators logic <= 3

generateLogic :: StdGen -> Logic
generateLogic = generateLogicWith defaultConfig
   
generateLogicWith :: LogicGenConfig -> StdGen -> Logic
generateLogicWith config stdGen =
   generate 0 stdGen (arbLogic config)
   
data LogicGenConfig = LogicGenConfig
   { maxDepth      :: Int
   , differentVars :: Int
   , freqConstant  :: Int
   , freqVariable  :: Int
   , freqImpl      :: Int
   , freqEquiv     :: Int
   , freqAnd       :: Int
   , freqOr        :: Int
   , freqNot       :: Int
   }
 deriving Show

defaultConfig :: LogicGenConfig
defaultConfig = LogicGenConfig
   { maxDepth      = 4
   , differentVars = 3
   , freqConstant  = 1
   , freqVariable  = 2
   , freqImpl      = 2
   , freqEquiv     = 2
   , freqAnd       = 3
   , freqOr        = 3
   , freqNot       = 3
   }

freqLeaf :: LogicGenConfig -> Int
freqLeaf config = freqConstant config + freqVariable config

arbLogic :: LogicGenConfig -> Gen Logic
arbLogic config
   | maxDepth config == 0 = arbLogicLeaf config
   | otherwise            = arbLogicBin  config

arbLogicLeaf :: LogicGenConfig -> Gen Logic
arbLogicLeaf config = frequency
   [ (freqConstant config, oneof $ map return [F, T])
   , (freqVariable config, oneof [ return (Var x) | x <- take (differentVars config) variableList])
   ]

arbLogicBin :: LogicGenConfig -> Gen Logic
arbLogicBin config = frequency
   [ (freqLeaf  config, arbLogicLeaf config)
   , (freqImpl  config, op2 (:->:))
   , (freqEquiv config, op2 (:<->:))
   , (freqAnd   config, op2 (:&&:))
   , (freqOr    config, op2 (:||:))
   , (freqNot   config, op1 Not)
   ]
 where
   rec   = arbLogic config {maxDepth = maxDepth config - 1}
   op1 f = liftM  f rec
   op2 f = liftM2 f rec rec

variableList :: [String]
variableList = ["p", "q", "r", "s", "t"] ++ [ "x" ++ show n | n <- [0..] ] 
   
{-
import Random (mkStdGen, randomRs)
import Logic.Solver.LogicParser

import Logic hiding (parseLogic)

type MinOp = Int  -- A boundary (!!) of the minimal number of operators in a formula 
type MaxOp = Int  -- A boundary (!!) of the maximal number of operators in a formula 

minop :: MinOp
minop = 1
maxop :: MaxOp
maxop = 2


type Seed  = Int  -- The seed used by the random generator


-- The logical operators used in the formulae. 
type OpList = [String]
operatorList :: OpList
operatorList = [ "~"     -- not
               , "->"    -- implication
               , "<->"   -- equivalence
               , "/\\"   -- and
               , "||"    -- or
               , "var"   -- variable
               ]


-- List of indices to point operators in operatorList
-- Example: [1, 0, 4] means ["->", "~", "||"]
type OpIndices  = [Int]

--  List of indices to point variables in varList
--  Example: [2, 3, 0] means ["r, "s, "p"]  
type VarIndices = [Int]  

-- Type brackets is used to determine whether or not brackets are needed
-- in around a formula. 
type Brackets = Bool


{-------------------------------------------------------------------
Function 'generateForm generates a new formula which is not a dnf. 
--------------------------------------------------------------------} 

-- Generate a formula and check wether the formula is a dnf
-- and contains minimal two binary operators. If not,
-- generate a new one with seed plus 2.

generateLogic :: Seed -> String
-- generateLogic s = "(~q || ~p) /\\ q"

-- "(t /\\ s) /\\ (s || q ) /\\ (s || q)"


generateLogic s = let f  = generateLogic' s
                      pf = parseLogic f
                  in if isDNF (pf) ||
                          not (containsTwoOrMoreBinOperators pf) ||
                          containsTwoOrMoreEquivaleceOperators pf 
                       then generateLogic (s+2)
                       else f
 


-- Generate a formula
generateLogic' s =
 let -- Determine the number of operators 
     numofop      = numOfOperators minop maxop s
     -- Determine the indices of the operators in the list of operators
     oplocList    = opLocators (s+1)
     -- Determine the indices of the variables in the list of variables
     varlocList   = varLocators (s+2)     
 in  makeLogic numofop oplocList varlocList False



-- Function "makeLogic" can be probably simplified. 

-- makeLogic :: Int -> OpIndices -> VarIndices -> Brackets -> String  
makeLogic 0 (o:os)     (v:vs)     _        = varList !! v           
makeLogic n (o1:o2:os) (v1:v2:vs) brackets =
   let -- Choice an operator 
       op = operatorList !! o1
   in  case op of
         "~"   -> if n==1
                  then "~" ++ makeLogic (n-1) os vs False
                  else "~" ++ "(" ++ makeLogic n os vs False ++ ")"
                   
         "->"  -> if brackets 
                  then "(" ++ makeLogic (n-1) (o2:os) (v2:vs) True ++ " -> " ++ makeLogic (n-1) os vs True ++ ")"
                  else        makeLogic (n-1) (o2:os) (v2:vs) True ++ " -> " ++ makeLogic (n-1) os vs True
                  
         "<->" -> if brackets   
                  then "(" ++ makeLogic (n-1) (o2:os) (v2:vs) True ++ " <-> " ++ makeLogic (n-1) os vs True ++ ")"
                  else        makeLogic (n-1) (o2:os) (v2:vs) True ++ " <-> " ++ makeLogic (n-1) os vs True
         
         "/\\" -> if brackets
                  then "(" ++ makeLogic (n-1) (o2:os) (v2:vs) True ++ " /\\ " ++ makeLogic (n-1) os vs True ++ ")"
                  else        makeLogic (n-1) (o2:os) (v2:vs) True ++ " /\\ " ++ makeLogic (n-1) os vs True
                  
         "||"  -> if brackets 
                  then "(" ++ makeLogic (n-1) (o2:os) (v2:vs) True ++ " || " ++ makeLogic (n-1) os vs True ++ ")"
                  else        makeLogic (n-1) (o2:os) (v2:vs) True ++ " || " ++ makeLogic (n-1) os vs True
                  
         "var" -> if n > 1
                  -- To prevent we get a too small formula.  
                  then makeLogic n os vs brackets
                  else varList !! v1


-- Number of operators in a formula   
numOfOperators :: MinOp -> MaxOp -> Seed -> Int
numOfOperators min max s = head (tail (randomRs (min, max) (mkStdGen s)))

-- The indices of the variables in the list of variables 
varLocators  :: Seed -> [Int]
varLocators s = tail (randomRs (0, (length varList) -1) (mkStdGen s))

-- The indices of the operators in the list of operators
opLocators :: Seed -> [Int]
opLocators s = tail (randomRs (0, (length operatorList) -1) (mkStdGen s))

containsTwoOrMoreBinOperators :: Logic -> Bool
containsTwoOrMoreBinOperators f = (nobo f) > 1
  where
   -- Number of binairy operators
   nobo f =  
    case f of
      Var v         ->  0
      (f1 :->: f2)  ->  1 + nobo f1 + nobo f2
      (f1 :<->: f2) ->  1 + nobo f1 + nobo f2
      (f1 :&&: f2)  ->  1 + nobo f1 + nobo f2
      (f1 :||: f2)  ->  1 + nobo f1 + nobo f2
      Not f         ->  nobo f
      T             ->  0
      F             ->  0

containsTwoOrMoreEquivaleceOperators :: Logic -> Bool
containsTwoOrMoreEquivaleceOperators f = (noeo f) > 1
  where
   -- Number of equivalence operators
   noeo f =  
    case f of
      Var v         ->  0
      (f1 :->: f2)  ->  noeo f1 + noeo f2
      (f1 :<->: f2) ->  1 + noeo f1 + noeo f2
      (f1 :&&: f2)  ->  noeo f1 + noeo f2
      (f1 :||: f2)  ->  noeo f1 + noeo f2
      Not f         ->  noeo f
      T             ->  0
      F             ->  0
      
      
      -}