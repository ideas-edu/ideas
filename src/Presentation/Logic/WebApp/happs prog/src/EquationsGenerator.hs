{--------------------------------------------------- 
Copyright (c)        2006 - 2007 
Johan Jeuring and Harrie Passier
---------------------------------------------------}


module EquationsGenerator where


import Random    (mkStdGen, randomRs)
import Equations (determinant, VarList, Var)


type N        = Int  -- Number of equations and variables per equation
type Bound    = Int  -- Used by random generator
type Seed     = Int  -- Used by random generator
type Coef     = Int  -- 

varBound :: Int      -- The variables {x,y,z,...} are bounded
varBound = 5         -- between -varBound and +varBound





{-------------------------------------------------------------------
Utility functions for safe inits, tails, etc.
--------------------------------------------------------------------} 

safeinit message x = if null x then error ("safeinit: " ++ message) else init x 
safelast message x = if null x then error ("safelast: " ++ message) else last x
safehead message x = if null x then error ("safehead: " ++ message) else head x

{-------------------------------------------------------------------
Function 'generateEqs' generates a new solvable system of linear
equations. Input N defines the number of variables and the number of
equations; a system of N equations with N variables. GenerateEqs
uses a random generator with arguments Seed and Bound.

Ps. At the moment, only systems with two or more variables can be
generated. Two, because function determinant demands two or more
equations 
--------------------------------------------------------------------} 


-- generateEqs n b s = "2*x+2*y=10" ++ "\n" ++ "-2*x+2*y=2"

 
generateEqs :: N -> Bound -> Seed -> String
generateEqs n b s =  makeEqs varParts conParts  
   where coefs     = generateCoefs n b s 
         varParts  = map (\x -> zip x vars) (map (safeinit "generateEqs") coefs)  
         conParts  = map (safelast "generateEqs") coefs
         vars      :: VarList
         vars      =  ["x","y","z","a","b","c","d","e","f","g","h","i","j"
                     ,"k","l","m","n","o","p","q","r","s","t","u","v","w"]


-- Make a system of equations as a string
makeEqs :: [[(Coef, Var)]] -> [Coef] -> String
makeEqs xs ys = concat (map makeEq (zip xs ys))


-- Make an equation as a string
makeEq :: ([(Coef, Var)], Coef) -> String
makeEq (xs, y) = makeExpr xs ++ "=" ++ show y ++ "\n"


-- Make an expression as a string
makeExpr :: [(Coef, Var)] -> String
makeExpr xs = if zeroTerms xs then "0" else makeExpr' xs 
  where 
  makeExpr' []        = ""
  makeExpr' ((i,v):xs)= if i==0 
                        then makeExpr' xs
                        else makeFirstTerm (i,v) ++ concat (map makeNextTerm xs)
                        where
                        makeFirstTerm (i,v) = if i == 0 then "" else term (i,v)
                        makeNextTerm (i,v)
                         | i == 0    = ""
                         | i > 0     = "+" ++ term (i,v)
                         | otherwise = term (i,v)
  term (i,v) = show i ++ "*" ++ v
  zeroTerms = and . map ((==) 0 . fst)



-- Generate for each of the n equations (n+1) coefficients   
generateCoefs :: N -> Bound -> Seed -> [[Coef]]
generateCoefs n b s =
   if determinant varCoefs == 0
   then generateCoefs n b (s+1)
   else allCoefs    
   
   where
   -- Generate the coefficients of the variables.
   varCoefs            = take n (partitionInts n (randomInts b s)) 
   partitionInts i xs  = take i xs : partitionInts i (drop i xs)
   -- Generate the values of the variables.
   varValues           = take n (randomInts varBound (s+1))
   -- Calculate the values of the constants and put all coeffients together.
   allCoefs = calcCoefs varCoefs varValues
   -- Select the coefficients of the variables 
   calcCoefs vcs vs = map (\(vc',vs') -> vc' ++ [sum (zipWith (*) vc' vs')]) (map (\vc -> (vc,vs)) vcs)

         

{--------------------------------------------------------------------
Function randomInts produces a random stream of integers (i)
within a specified bound (-b <= i <= b) and starts with a particulair
seed.
---------------------------------------------------------------------}

 
randomInts :: Bound -> Seed -> [Int]
randomInts b s = tail (randomRs (-b, b) (mkStdGen s))




{---------------------------------------------------------------------
   Genereerd geen mooie stelsels (variabelen zijn meestal breuken. Kan
   wel als extra optie geboden worden: "moeilijker".
      
-- Generate for each of the n equations (n+1) coefficients   
generateCoefs :: N -> Bound -> Seed -> [[Coef]]
generateCoefs n b s =
   if determinant squareCoefs == 0
   then generateCoefs n (safehead "generateCoefs1" (safehead "generateCoefs2" coefs) + 1) b
   else coefs    
   where coefs = take n (partitionInts n (randomInts b s)) 
         squareCoefs         = map (safeinit "generateCoefs") coefs  
         partitionInts       :: Int -> [Int] -> [[Int]]
         partitionInts i xs  = take (i+1) xs : partitionInts i (drop (i+1) xs)
-----------------------------------------------------------------------}












