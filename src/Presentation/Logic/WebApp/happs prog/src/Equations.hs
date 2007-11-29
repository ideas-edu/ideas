{--------------------------------------------------- 
Copyright (c)        2005 - 2006 
Johan Jeuring and Harrie Passier
---------------------------------------------------}
module Equations where

-- Standard Haskell library
import List
import Maybe

-- GHC library
import GHC.Real


{- The `model' for a system of equations -}

infixl 6 :+:,:-:
infixr 7 :*:,:/:

type Equations  =  [Equation]
data Equation   =  Expr :=: Expr     deriving (Show,Eq)
data Expr       =  Zero
                |  Con Rational 
                |  Var String
                |  Expr :+: Expr
                |  Expr :-: Expr
                |  Expr :*: Expr
                |  Expr :/: Expr     deriving (Show,Eq,Ord)

type VarList    =  [Var]
type Var        =  String
type MaybeVar   =  Maybe String

type Solution   =  [Binding]
type Binding    =  (String,Expr)


{- Determines the factors in a term -}

listFactors :: Expr -> [Expr] 
listFactors (e1 :*: e2) = listFactors e1 ++ listFactors e2  
listFactors (e1 :/: e2) = listFactors e1 ++ listFactors (divExpr e2)
listFactors t           = [t]     

divExpr e = Con 1 :/: e

numOfFacTerm :: Expr -> Int
numOfFacTerm = length . listFactors

numOfFacExpr :: Expr -> Int
numOfFacExpr  = length . concatMap listFactors . listTerms 

numOfFacEq :: Equation -> Int
numOfFacEq (l:=:r) = numOfFacExpr l + numOfFacExpr r
 

{- Determine the terms in a expression -}

listTerms :: Expr -> [Expr] 
listTerms (e1 :+: e2) = listTerms e1 ++ listTerms e2  
listTerms (e1 :-: e2) = listTerms e1 ++ listTerms (negateExpr e2)
listTerms t           = [t]  

negateExpr (Con c)       = Con (-c)
negateExpr (Con c :*: e) = Con (-c) :*: e
negateExpr (Con c :/: e) = Con (-c) :/: e
negateExpr (e)           = Con (-1) :*: e

numOfTermsExpr :: Expr -> Int
numOfTermsExpr = length . listTerms

numOfTermsEq :: Equation -> Int
numOfTermsEq (l:=:r) = numOfTermsExpr l + numOfTermsExpr r


{- Determining subsets of expressions. -}

isCon          :: Expr -> Bool
isCon (Con c)  =  True
isCon _        =  False
 
isVar          :: Expr -> Bool
isVar (Var v)  =  True
isVar _        =  False

isConVar                    :: Expr -> Bool
isConVar (Con c :*: Var v)  =  True
isConVar _                  =  False 

leftExpr, rigthExpr :: Equation -> Expr
leftExpr  (l:=:r)  = l
rigthExpr (l:=:r)  = r

leftConVar :: Equation -> Bool
leftConVar = isConVar . leftExpr


{- Calculate the variables that occur in a system of equations. -}

varsEquations  ::  Equations -> VarList
varsEquations  =   concat . map varsEquation

varsEquation          ::  Equation -> VarList
varsEquation (l:=:r)  =   varsExpr l ++ varsExpr r

varsExpr       ::  Expr -> VarList
varsExpr expr  =   varsExpr' expr []
  where varsExpr'          ::  Expr -> VarList -> VarList 
        varsExpr' expr vl  = 
          case expr of
            Zero     ->  vl
            Con c    ->  vl
            Var v    ->  v:vl
            l :+: r  ->  varsExpr' l (varsExpr' r vl)
            l :-: r  ->  varsExpr' l (varsExpr' r vl)
            l :*: r  ->  varsExpr' l (varsExpr' r vl)
            l :/: r  ->  varsExpr' l (varsExpr' r vl)


{- Evaluate an expression to a rational. -}

evalExpr :: Expr -> Rational
evalExpr expr = 
  case expr of
    Zero     ->  0.0
    Con c    ->  c
    Var v    ->  error ("cannot calculate evalExpr")
    l :+: r  ->  evalExpr l + evalExpr r
    l :-: r  ->  evalExpr l - evalExpr r
    l :*: r  ->  evalExpr l * evalExpr r
    l :/: r  ->  evalExpr l / evalExpr r


{- Coefficient determines the coefficient of a variable in an Equation. -}

coefficient :: Var -> Equation -> Rational
coefficient v (l:=:r) = coefficientExpr v l - coefficientExpr v r

coefficientExpr :: Var -> Expr -> Rational
coefficientExpr v expr = 
  case expr of
    Zero     ->  0
    Con c    ->  0
    Var v'   ->  if v == v' then 1 else 0
    l :+: r  ->  if v `elem` varsExpr l && v `elem` varsExpr r 
                 then coefficientExpr v l + coefficientExpr v r
                 else if v `elem` varsExpr l
                      then coefficientExpr v l 
                      else if v `elem` varsExpr r
                           then coefficientExpr v r
                           else 0
    l :-: r  ->  if v `elem` varsExpr l && v `elem` varsExpr r 
                 then coefficientExpr v l - coefficientExpr v r
                 else if v `elem` varsExpr l
                      then coefficientExpr v l 
                      else if v `elem` varsExpr r
                           then negate (coefficientExpr v r)
                           else 0
    l :*: r  ->  if v `elem` varsExpr l && v `elem` varsExpr r 
                 then error ("coefficientExpr: Non-linear equation: ")
                 else if v `elem` varsExpr l
                      then coefficientExpr v l * evalExpr r
                      else if v `elem` varsExpr r
                           then evalExpr l * coefficientExpr v r
                           else 0
    l :/: r  ->  if v `elem` varsExpr l && v `elem` varsExpr r 
                 then error ("coefficientExpr: Non-linear equation: ")
                 else if v `elem` varsExpr l
                      then coefficientExpr v l / evalExpr r
                      else if v `elem` varsExpr r
                           then evalExpr l / coefficientExpr v r
                           else 0


sizeEquations  :: Equations -> Int
sizeEquations  =  sum . map sizeEquation


sizeEquation          :: Equation -> Int
sizeEquation (l:=:r)  =  sizeExpr l + sizeExpr r


{- Calculate the size of an expression, counting one for each constructor. -}

sizeExpr :: Expr -> Int
sizeExpr expr = 
  case expr of
    Zero    ->  1
    Con c   ->  1
    Var v   ->  1
    l :+: r ->  1 + sizeExpr l + sizeExpr r
    l :-: r ->  1 + sizeExpr l + sizeExpr r
    l :*: r ->  1 + sizeExpr l + sizeExpr r
    l :/: r ->  1 + sizeExpr l + sizeExpr r


{- Rewrite an equation in standard form: ax + by = c -}

symbolicEvalEquation :: Equation -> Equation
symbolicEvalEquation eq = 
    let lhs = makeExpr (varPartEquation eq)  0
        rhs = makeExpr []                    (conPartEquation eq)
    in  (lhs :=: rhs)   
    where 
    varPartEquation           :: Equation -> [(Rational, Var)] 
    varPartEquation eq        = removeZeroTerms [(coefficient v eq, v) | v <- (nub . varsEquation) eq]
    conPartEquation           :: Equation -> Rational
    conPartEquation (l :=: r) = (conPartExpr r - conPartExpr l)

removeZeroTerms  :: [(Rational, Var)] -> [(Rational, Var)]
removeZeroTerms  =  filter (\(x,y) -> x/=0) 


{- Are two expressions semantically equal? -}

equalExprs :: Expr -> Expr -> Bool
equalExprs expr1 expr2 = symbolicEvalExpr (expr1 :-: expr2) == Con 0


{- Rewrite an expression in standard form: ax + by + c -}

symbolicEvalExpr :: Expr -> Expr
symbolicEvalExpr expr  = makeExpr (varPartExpr expr) (conPartExpr expr)


{- Determine the variable part of an expression: conPart (ax+by+c) is c -}

varPartExpr       :: Expr -> [(Rational, Var)] 
varPartExpr expr  =  removeZeroTerms [(coefficientExpr v expr, v) | v <- (nub . varsExpr) expr]


{- Determine the constant part of an expression: conPart (ax+by+c) is c -}

conPartExpr :: Expr -> Rational
conPartExpr expr = 
  case expr of
    Zero     ->  0
    Con c    ->  c
    Var v    ->  0
    l :+: r  ->  conPartExpr l + conPartExpr r
    l :-: r  ->  conPartExpr l - conPartExpr r
    l :*: r  ->  conPartExpr l * conPartExpr r
    l :/: r  ->  conPartExpr l / conPartExpr r
   

makeExpr :: [(Rational, Var)] -> Rational -> Expr    
makeExpr varPart conPart 
      | null varPart = Con conPart 
      | otherwise    = if    conPart == 0
                       then  makeVarExpr varPart
                       else  (makeVarExpr varPart) :+: (Con conPart) 
      where
      makeVarExpr [(c, v)]   =  if c==1 then Var v else Con c :*: Var v   
      makeVarExpr ((c,v):fs) =  if    c==1 
                                then  Var v :+: makeVarExpr fs 
                                else  (Con c :*: Var v) :+: makeVarExpr fs  

{- Check bindings. -}

isBinding                    :: Equation -> Bool
isBinding (Var v :=: _)      =  True
isBinding _                  =  False

bindings  =  filter isBinding

boundVars = map (\(Var v :=: r) -> v) . bindings

isBindingFor                   :: Var -> Equation -> Bool
isBindingFor v' (Var v :=: _)  =  v == v'
isBindingFor v' _              =  False

bindingsFor    :: Var -> Equations -> [Equation]
bindingsFor v  =  filter (isBindingFor v) 


isSolved                     :: Equation -> Bool
isSolved (Var v :=: Con c)  =  True
isSolved _                  =  False

isSolvedFor                       :: Var -> Equation -> Bool
isSolvedFor v' (Var v :=: Con c)  =  v == v'
isSolvedFor v' _                  =  False

solvedEquations      :: Equations -> Bool
solvedEquations sle  = all (\v -> any (isSolvedFor v) sle) vars 
  where vars = nub (varsEquations sle)
 

{--------------------------------------------------- 
Function analyse checks that:

- the number of variables is at most the number
  of equations;
- all equations are linear.
---------------------------------------------------}

analyse :: Equations -> (Bool,Bool)
analyse equations = 
  ( length (nub (varsEquations equations)) <= length equations
  , all linear equations
  )
  
linear          ::  Equation -> Bool
linear (l:=:r)  =   linearExpr l && linearExpr r

linearExpr  ::  Expr -> Bool
linearExpr expr = 
  case expr of
    Zero     ->  True
    Con c    ->  True
    Var v    ->  True
    l :+: r  ->  linearExpr l && linearExpr r
    l :-: r  ->  linearExpr l && linearExpr r
    l :*: r  ->  null (varsExpr l) || null (varsExpr r)
    l :/: r  ->  null (varsExpr l) || null (varsExpr r)


solvable :: Equations -> Bool
solvable equations = 
  let vars = nub (varsEquations equations)
      matrix = map (\equation -> map (\v -> coefficient v equation) vars) equations 
  in determinant matrix /= 0


{- I assume the matrix is square when determinant is called. -}
determinant :: Num a => [[a]] -> a
determinant (x:xs) 
  | length x == 2  =  let y = if    not (null xs)
                              then  head xs
                              else  error "determinant: applied to a non-square matrix"
                      in if    length y == 2
                         then  head x * head (tail y) - head (tail x) * head y
                         else  error "determinant: applied to a non-square matrix"
  | length x > 2   =  let columns               =  transpose xs
                          smaller_matrices      =  delete_one columns
                          smaller_determinants  =  map determinant smaller_matrices
                      in  alternate (-) (+) (zipWith (*) x smaller_determinants)
  | otherwise      =  error "determinant: applied to a non-matrix"

determinant []     =  error "determinant: no determinant of []"  


delete_one         :: [a] -> [[a]]
delete_one []      =  error "delete_one: impossible to delete an element from the empty list"
delete_one [x]     =  []
delete_one (x:xs)  =  xs:map (x:) (delete_one xs)

alternate op1 op2 []      =  error "alternate: empty argument"
alternate op1 op2 [x]     =  x
alternate op1 op2 (x:xs)  =  x `op1` alternate op2 op1 xs



{---------------------------------------------------------------
solveEquations solves a system of equations from left to right.
A solution consists of equations with a single variable to the
left, and an expression that may contain variables to the right.
The variables in the rhs expression have not been solved yet.

The solution is accumulated in solvedEquations, and the 
variables that appear at the lhs in the solution are accumulated
in vars.

At each step, the current solution is used to substitute away
variables in an equation, and the resulting equation is used
to substitute away occurrences of the new variable in the
existing solution.
---------------------------------------------------------------}

solveEquations :: Equations -> Solution
solveEquations = solveEquations' [] [] where 
  solveEquations'  ::  VarList -> Solution -> Equations -> Solution
  solveEquations' vars solvedEquations []                    =  solvedEquations
  solveEquations' vars solvedEquations (equation:equations)  =
          let equation'         =  solveEquation vars (substSolution solvedEquations equation)
              solvedEquations'  =  map (\(x,y) -> (x,substExpr [equation'] y)) solvedEquations
          in solveEquations' (fst equation':vars) (equation':solvedEquations') equations


{---------------------------------------------------------------
solveEquations solves a single equation. It takes a list of
variables that already appear at the lhs in the current solution.

It first calculates the variable with respect to which the 
equation is solved: the first variable that does not occur in
vars. Then it calculates how often this var occurs, and it
removes the var in both the lhs as the rhs expression. Finally,
it returns the obtained rhs expression subtracted with the
lhs expression, and divided by the coefficient.
---------------------------------------------------------------}

solveEquation :: VarList -> Equation -> Binding
solveEquation vars equation@(l:=:r)  = 
  let var    =  fromJust (newVar vars equation)
      coef   =  coefficient var equation
      left   =  removeVar var l
      right  =  removeVar var r
   in (var,(right :-: left) :/: Con coef)
  

newVar :: VarList -> Equation -> Maybe Var
newVar vars (l:=:r) = 
  case newVarExpr vars l of
    Just v   ->  Just v
    Nothing  ->  newVarExpr vars r
    

newVarExpr :: VarList -> Expr -> Maybe Var
newVarExpr vars expr = 
  case expr of
    Zero     ->  Nothing
    Con c    ->  Nothing
    Var v    ->  if v `elem` vars then Nothing else Just v
    l :+: r  ->  left_to_right vars l r
    l :-: r  ->  left_to_right vars l r
    l :*: r  ->  left_to_right vars l r
    l :/: r  ->  left_to_right vars l r
    where left_to_right vars l r =  case newVarExpr vars l of 
                                      Just v   ->  Just v
                                      Nothing  ->  newVarExpr vars r
                           
removeVar :: Var -> Expr -> Expr
removeVar var expr = 
  case expr of
    Zero     ->  Zero
    Con c    ->  Con c
    Var v    ->  if v==var then Zero else Var v
    l :+: r  ->  removeVar var l :+: removeVar var r
    l :-: r  ->  removeVar var l :-: removeVar var r
    l :*: r  ->  removeVar var l :*: removeVar var r
    l :/: r  ->  removeVar var l :/: removeVar var r
  

substSolution :: Solution -> Equation -> Equation
substSolution s (l :=: r) = (substExpr s l :=: substExpr s r)


substExpr :: Solution -> Expr -> Expr
substExpr solution expr =
  case expr of 
    Var s    ->  maybe (Var s) id (lookup s solution)
    l :+: r  ->  substExpr solution l :+: substExpr solution r
    l :-: r  ->  substExpr solution l :-: substExpr solution r
    l :*: r  ->  substExpr solution l :*: substExpr solution r
    l :/: r  ->  substExpr solution l :/: substExpr solution r
    x        ->  x
 

evalSolution :: Solution -> [(String,Rational)]
evalSolution = map (\(x,y) -> (x,evalExpr y))