-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Ideas.Main.IDEAS (useIDEAS) where

import Ideas.Common.Exercise
import Ideas.Common.Id
import Ideas.Common.Utils (Some(..))
import Control.Arrow
import Ideas.Main.Options
import Ideas.Service.DomainReasoner
import Ideas.Service.ServiceList

useIDEAS :: DomainReasoner a -> IO a
useIDEAS action = runDomainReasoner $ do
   -- version information
   setVersion     shortVersion
   setFullVersion fullVersion
   -- exercises
   addExercises   exercises
   addAliases     aliases
   -- services
   addServices  serviceList
   addExerciseService exerciselistS
   -- views
   -- addViews Math.viewList
   -- feedback scripts
   flags <- liftIO serviceOptions
   setScriptDir (scriptDir flags)
   addScripts scripts
   -- domain checks
   -- addTestSuite $ do
   --   sequence_ Math.testSuiteList
   --   LA.checks
   -- do the rest
   action

exercises :: [Some Exercise]
exercises = [] {- 
   [ -- logic and relation-algebra
     Some Logic.dnfExercise
   , Some Logic.dnfUnicodeExercise
   , Some Logic.proofExercise
   , Some Logic.proofUnicodeExercise
   , Some RA.cnfExercise
     -- linear algebra
   , Some LA.gramSchmidtExercise
   , Some LA.linearSystemExercise
   , Some LA.gaussianElimExercise
   , Some LA.systemWithMatrixExercise
     -- regular expressions
   -- , some RE.regexpExercise
   ] ++ Math.exerciseList -}

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
scripts = [] {- 
   [ (getId Logic.dnfExercise,         "logic.txt")
   , (getId Logic.dnfUnicodeExercise,  "logic.txt")
   ] ++ Math.scriptList -}