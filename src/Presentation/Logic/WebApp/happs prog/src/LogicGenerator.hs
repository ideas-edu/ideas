{------------------------------------------------------------------ 
Copyright (c)        2006 - 2007 
Johan Jeuring and Harrie Passier
------------------------------------------------------------------}


module LogicGenerator where


import Random (mkStdGen, randomRs)
import LogicFormula
import LogicParser


type MinOp = Int  -- A boundary (!!) of the minimal number of operators in a formula 
type MaxOp = Int  -- A boundary (!!) of the maximal number of operators in a formula 

minop :: MinOp
minop = 1
maxop :: MaxOp
maxop = 2


type Seed  = Int  -- The seed used by the random generator


-- The names of the variables used in the formulae.
varList :: VarList
varList = ["p", "q", "r", "s", "t"] 


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

generateFormula :: Seed -> String
-- generateFormula s = "(~q || ~p) /\\ q"

-- "(t /\\ s) /\\ (s || q ) /\\ (s || q)"


generateFormula s = let f  = generateFormula' s
                        pf = parseFormula f
                    in if isDnf (pf) ||
                          not (containsTwoOrMoreBinOperators pf) ||
                          containsTwoOrMoreEquivaleceOperators pf 
                       then generateFormula (s+2)
                       else f
 


-- Generate a formula
generateFormula' s =
 let -- Determine the number of operators 
     numofop      = numOfOperators minop maxop s
     -- Determine the indices of the operators in the list of operators
     oplocList    = opLocators (s+1)
     -- Determine the indices of the variables in the list of variables
     varlocList   = varLocators (s+2)     
 in  makeFormula numofop oplocList varlocList False



-- Function "makeFormula" can be probably simplified. 

-- makeFormula :: Int -> OpIndices -> VarIndices -> Brackets -> String  
makeFormula 0 (o:os)     (v:vs)     _        = varList !! v           
makeFormula n (o1:o2:os) (v1:v2:vs) brackets =
   let -- Choice an operator 
       op = operatorList !! o1
   in  case op of
         "~"   -> if n==1
                  then "~" ++ makeFormula (n-1) os vs False
                  else "~" ++ "(" ++ makeFormula n os vs False ++ ")"
                   
         "->"  -> if brackets 
                  then "(" ++ makeFormula (n-1) (o2:os) (v2:vs) True ++ " -> " ++ makeFormula (n-1) os vs True ++ ")"
                  else        makeFormula (n-1) (o2:os) (v2:vs) True ++ " -> " ++ makeFormula (n-1) os vs True
                  
         "<->" -> if brackets   
                  then "(" ++ makeFormula (n-1) (o2:os) (v2:vs) True ++ " <-> " ++ makeFormula (n-1) os vs True ++ ")"
                  else        makeFormula (n-1) (o2:os) (v2:vs) True ++ " <-> " ++ makeFormula (n-1) os vs True
         
         "/\\" -> if brackets
                  then "(" ++ makeFormula (n-1) (o2:os) (v2:vs) True ++ " /\\ " ++ makeFormula (n-1) os vs True ++ ")"
                  else        makeFormula (n-1) (o2:os) (v2:vs) True ++ " /\\ " ++ makeFormula (n-1) os vs True
                  
         "||"  -> if brackets 
                  then "(" ++ makeFormula (n-1) (o2:os) (v2:vs) True ++ " || " ++ makeFormula (n-1) os vs True ++ ")"
                  else        makeFormula (n-1) (o2:os) (v2:vs) True ++ " || " ++ makeFormula (n-1) os vs True
                  
         "var" -> if n > 1
                  -- To prevent we get a too small formula.  
                  then makeFormula n os vs brackets
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

containsTwoOrMoreBinOperators :: Formula -> Bool
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

containsTwoOrMoreEquivaleceOperators :: Formula -> Bool
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