-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Main.IDEAS (useIDEAS) where

import Common.Id
import Common.Utils (Some(..))
import Control.Arrow
import Main.Options
import Service.DomainReasoner
import Service.ExercisePackage
import Service.ServiceList
import qualified Domain.LinearAlgebra as LA
import qualified Domain.LinearAlgebra.Checks as LA
import qualified Domain.Logic as Logic
import qualified Domain.Math.Data.Interval as MathInterval
import qualified Domain.Math.Derivative.Exercises as Math
import qualified Domain.Math.Equation.CoverUpExercise as Math
import qualified Domain.Math.Numeric.Exercises as Math
import qualified Domain.Math.Numeric.Tests as MathNum
import qualified Domain.Math.Polynomial.Exercises as Math
import qualified Domain.Math.Polynomial.IneqExercises as Math
import qualified Domain.Math.Polynomial.RationalExercises as Math
import qualified Domain.Math.Polynomial.Balance as Math
import qualified Domain.Math.Polynomial.Tests as MathPoly
import qualified Domain.Math.Power.Exercises as Math
import qualified Domain.Math.Power.Equation.Exercises as Math
import qualified Domain.Math.SquareRoot.Tests as MathSqrt
import qualified Domain.Math.Polynomial.LeastCommonMultiple as MathLCM
import qualified Domain.RelationAlgebra as RA

useIDEAS :: DomainReasoner a -> IO a
useIDEAS action = runDomainReasoner $ do
   -- version information
   setVersion     shortVersion
   setFullVersion fullVersion
   -- exercise packages
   addPackages    packages
   addAliases     aliases
   -- services
   addServices    serviceList
   addPkgService  exerciselistS
   -- feedback scripts
   flags <- liftIO serviceOptions
   setScriptDir (scriptDir flags)
   addScripts scripts
   -- domain checks
   addTestSuite $ do
      MathNum.main
      MathPoly.tests
      MathSqrt.tests
      MathInterval.testMe
      MathLCM.testLCM
      LA.checks
   -- do the rest
   action

packages :: [Some ExercisePackage]
packages =
   [ -- logic and relation-algebra
     someTermPackage Logic.dnfExercise
   , someTermPackage Logic.dnfUnicodeExercise
   -- , somePackage Logic.proofExercise
   , somePackage RA.cnfExercise
     -- basic math
   -- , someTermPackage Math.naturalExercise
   -- , someTermPackage Math.integerExercise
   -- , someTermPackage Math.rationalExercise
   , someTermPackage Math.fractionExercise
   , someTermPackage Math.coverUpExercise
   , someTermPackage Math.linearExercise
   , someTermPackage Math.linearMixedExercise
   , someTermPackage Math.balanceExercise
   , someTermPackage Math.quadraticExercise
   , someTermPackage Math.higherDegreeExercise
   , someTermPackage Math.findFactorsExercise
   , someTermPackage Math.ineqLinearExercise
   , someTermPackage Math.ineqQuadraticExercise
   , someTermPackage Math.ineqHigherDegreeExercise
   , someTermPackage Math.rationalEquationExercise
   , someTermPackage Math.simplifyRationalExercise
   -- , someTermPackage Math.divisionBrokenExercise
   , someTermPackage Math.quadraticNoABCExercise
   , someTermPackage Math.quadraticWithApproximation
   , someTermPackage Math.derivativeExercise
   , someTermPackage Math.derivativePolyExercise
   , someTermPackage Math.derivativeProductExercise
   , someTermPackage Math.derivativeQuotientExercise
   -- , someTermPackage Math.derivativePowerExercise
   , someTermPackage Math.simplifyPowerExercise
   , someTermPackage Math.powerOfExercise     
   , someTermPackage Math.nonNegBrokenExpExercise
   , someTermPackage Math.calcPowerExercise
   , someTermPackage Math.powerEqExercise
   , someTermPackage Math.expEqExercise
   , someTermPackage Math.logEqExercise
--   , someTermPackage Math.higherPowerEqExercise
     -- linear algebra
   , someTermPackage LA.gramSchmidtExercise
   , someTermPackage LA.linearSystemExercise
   , someTermPackage LA.gaussianElimExercise
   , someTermPackage LA.systemWithMatrixExercise
     -- regular expressions
   -- , somePackage RE.regexpExercise
   ]
   
aliases :: [(Id, Id)]
aliases = map (newId *** newId)
   [ ("math.coverup",             "algebra.equations.coverup")
   , ("math.lineq",               "algebra.equations.linear")
   , ("math.lineq-mixed",         "algebra.equations.linear.mixed")
   , ("math.quadreq",             "algebra.equations.quadratic")         
   , ("math.quadreq-no-abc",      "algebra.equations.quadratic.no-abc")    
   , ("math.quadreq-with-approx", "algebra.equations.quadratic.approximate")
   , ("math.higherdegree",        "algebra.equations.polynomial")
   , ("math.rationaleq",          "algebra.equations.rational")
   , ("math.linineq",             "algebra.inequalities.linear")
   , ("math.quadrineq",           "algebra.inequalities.quadratic")
   , ("math.ineqhigherdegree",    "algebra.inequalities.polynomial")
   , ("math.factor",              "algebra.manipulation.polynomial.factor")
   , ("math.simplifyrational",    "algebra.manipulation.rational.simplify")
   , ("math.simplifypower",       "algebra.manipulation.exponents.simplify")
   , ("math.nonnegexp",           "algebra.manipulation.exponents.nonnegative")
   , ("math.powerof",             "algebra.manipulation.exponents.powerof")
   , ("math.derivative",          "calculus.differentiation")
   , ("math.fraction",            "arithmetic.fractions")
   , ("math.calcpower",           "arithmetic.exponents")
   , ("linalg.gaussianelim",      "linearalgebra.gaussianelim")
   , ("linalg.gramschmidt",       "linearalgebra.gramschmidt")
   , ("linalg.linsystem",         "linearalgebra.linsystem")
   , ("linalg.systemwithmatrix",  "linearalgebra.systemwithmatrix")
   , ("logic.dnf",                "logic.propositional.dnf")
   , ("logic.dnf-unicode",        "logic.propositional.dnf.unicode")
   , ("relationalg.cnf",          "relationalgebra.cnf")
   -- MathDox compatibility
   , ("gaussianelimination"        , "linearalgebra.gaussianelim")
   , ("gramschmidt"                , "linearalgebra.gramschmidt")
   , ("solvelinearsystem"          , "linearalgebra.linsystem")
   , ("solvelinearsystemwithmatrix", "linearalgebra.systemwithmatrix")
   ]
   
scripts :: [(Id, FilePath)]
scripts =
   [ (getId Logic.dnfExercise,         "logic.txt")
   , (getId Logic.dnfUnicodeExercise,  "logic.txt")
   , (getId Math.linearExercise,       "math.lineq-en.txt")
   , (getId Math.quadraticExercise,    "math.quadreq-en.txt")
   , (getId Math.higherDegreeExercise, "math.polyeq-en.txt")
   ]      