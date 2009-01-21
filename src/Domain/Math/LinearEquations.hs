module Domain.Math.LinearEquations where

import Common.Strategy
import Domain.Math.Expr
import Domain.Math.Symbolic
import Domain.Math.HigherDegreeEquations (solve, OrList(..)) -- to reuse some helpers (temp)
import Domain.LinearAlgebra.Equation

----------------------------------------------------------------------
-- Linear Equations Exercise Sets (from DWO environment)

main = putStrLn $ unlines $ map show $ map (\n -> 
     runStrategy solve (OrList [level1 !! n])) [0..9]

level1 :: [Equation Expr]
level1 = 
   let x = variable "x" in
   [ 5*x + 3   :==: 18
   , 11*x - 12 :==: 21
   , 19 - 3*x  :==: -5
   , -12 + 5*x :==: 33
   , 15 - 9*x  :==: 6
   , 4*x + 18  :==: 0
   , 11*x - 12 :==: -34
   , -2*x - 3  :==: -4
   , 6*x - 12  :==: 2
   , -4*x - 13 :==: -11
   ]

level2 :: [Equation Expr]
level2 = 
   let x = variable "x" in
   [ 6*x-2    :==: 2*x+14
   , 3+6*x    :==: 3*x+24
   , 5*x+7    :==: 2*x - 10
   , 2*x-8    :==: 18 - x
   , 4*x - 6  :==: 7*x - 14
   , -1 -5*x  :==: 3*x - 20
   , 4*x - 7  :==: -5*x - 24
   , 4*x - 18 :==: 14 + 11*x
   , 17       :==: 4 - 10*x
   , -5*x + 6 :==: 2 - 3*x
   ]

level3 :: [Equation Expr]
level3 = 
   let x = variable "x" in
   [ 4*(x-1)          :==: 11*x - 12
   , 4*(x-4)          :==: 5*(2*x+1)
   , 2*(5-3*x)        :==: 6-x
   , 4*x - (x-2)      :==: 12 + 5*(x-1)
   , -3*(x-2)         :==: 3*(x+4) - 7
   , 3*(4*x-1) + 3    :==: 7*x - 14
   , 4*(4*x - 1) - 2  :==: -3*x + 3*(2*x -5)
   , 2*x - (3*x + 5)  :==: 10 + 5*(x-1)
   , -5*(x+1)         :==: 9*(x+4)-5
   , 18 - 2*(4*x + 2) :==: 7*x - 4*(4*x -2)
   ]

level4 :: [Equation Expr]
level4 = 
   let x = variable "x" in
   [ (1/2)*x - 4            :==: 2*x + 2+(1/2)
   , (1/4)*x + (1/2)        :==: (5/2)*x + 2
   , (1/4)*x - (3/4)        :==: 2*x + (1/2)
   , -(1/2)*x + (3/4)       :==: (5/2)*x + 3
   , -(1/2)*x + 1+(1/2)     :==: 2*x - 5
   , -(1/3)*x + (3/4)       :==: (1/4)*x + (1/6)
   , (3/4)*x - (1/3)        :==: (2/3)*x - (3/4)
   , (2/5)*x - (1/4)        :==: (1/2)*x + (3/4)
   , (2/3)*x - 2            :==: (1/5)*x - (3/5)
   , (-1+(2/5))*x + 3+(1/2) :==: (3/5)*x + (9/10)
   ]

level5 :: [Equation Expr]
level5 = 
   let x = variable "x" in
   [ (1/4)*(x-3)         :==: (1/2)*x - 4
   , (x+3)/2             :==: 5*((1/2)*x + 1 + (1/2))
   , (1/2)*(7-(2/3)*x)   :==: 2 + (1/9)*x
   , (3/4)*x - (x-1)     :==: 3 + (2+(1/2))*(x-1)
   , -(5/4)*(x-7)        :==: (3/4)*(x+2) - (4+(1/2))
   , 3*((1/5)*x - 1) + 5 :==: 7*x - 14
   , ((5*x - 1) / 6) - 2 :==: -4*x + (3*x - 6)/2
   , 2*x - ((2*x+2)/5)   :==: 12 + (x-1)/6
   , (-3*(x+2))/6        :==: 9*((2/3)*x + (1/3)) - (5/3)
   , 1 - ((4*x + 2)/3)   :==: 3*x - ((5*x - 1) / 4)
   ]