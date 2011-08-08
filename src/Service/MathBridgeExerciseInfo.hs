module Service.MathBridgeExerciseInfo where

import Data.Map(Map,empty,insert)

import Common.Library

import Domain.LinearAlgebra
import Domain.Math.Polynomial.Exercises
import Domain.Math.Derivative.Exercises
import Domain.Math.Numeric.Exercises
import Domain.Math.Polynomial.RationalExercises
import Domain.Math.Polynomial.IneqExercises
import Domain.Math.Equation.CoverUpExercise
import Domain.Math.Power.Exercises
import Domain.Math.Power.Equation.Exercises

--------------------------------------------------------------------------------
{- Different languages can be supported.
-}
--------------------------------------------------------------------------------

data Lang = EN | ES | DE | FR | NL | FI | ALL 

instance Show Lang where
  show EN = "en"
  show ES = "es"
  show DE = "de"
  show FR = "fr"
  show NL = "nl"
  show FI = "fi"

--------------------------------------------------------------------------------
{- Info about exercises for ActiveMath
-}
--------------------------------------------------------------------------------

data MBExerciseInfo = MBExerciseInfo
  { title             :: String
  , for               :: String
  , langSupported     :: [Lang]
  , cmp               :: Lang -> String
  , problemStatement  :: String
  , context           :: String
  , difficulty        :: String
  }

mBExerciseInfo :: Map Id MBExerciseInfo
mBExerciseInfo =  
    insert (exerciseId coverUpExercise)            coverUpExerciseInfo
  $ insert (exerciseId derivativeExercise)         derivativeExerciseInfo
  $ insert (exerciseId gramSchmidtExercise)        gramSchmidtExerciseInfo
  $ insert (exerciseId linearSystemExercise)       linearSystemExerciseInfo
  $ insert (exerciseId gaussianElimExercise)       gaussianElimExerciseInfo
  $ insert (exerciseId systemWithMatrixExercise)   systemWithMatrixExerciseInfo
  $ insert (exerciseId powerOfExercise)            powerOfExerciseInfo
  $ insert (exerciseId nonNegBrokenExpExercise)    nonNegBrokenExpExerciseInfo
  $ insert (exerciseId calcPowerExercise)          calcPowerExerciseInfo
  $ insert (exerciseId powerEqExercise)            powerEqExerciseInfo
  $ insert (exerciseId expEqExercise)              expEqExerciseInfo
  $ insert (exerciseId logEqExercise)              logEqExerciseInfo
  $ insert (exerciseId derivativePolyExercise)     derivativePolyExerciseInfo
  $ insert (exerciseId derivativeProductExercise)  derivativeProductExerciseInfo
  $ insert (exerciseId derivativeQuotientExercise) derivativeQuotientExerciseInfo
  $ insert (exerciseId findFactorsExercise)        findFactorsExerciseInfo
  $ insert (exerciseId fractionExercise)           fractionExerciseInfo
  $ insert (exerciseId higherDegreeExercise)       higherDegreeExerciseInfo
  $ insert (exerciseId ineqHigherDegreeExercise)   ineqHigherDegreeExerciseInfo
  $ insert (exerciseId ineqLinearExercise)         ineqLinearExerciseInfo
  $ insert (exerciseId ineqQuadraticExercise)      ineqQuadraticExerciseInfo
  $ insert (exerciseId linearExercise)             linearExerciseInfo 
  $ insert (exerciseId linearMixedExercise)        linearMixedExerciseInfo
  $ insert (exerciseId quadraticExercise)          quadraticExerciseInfo
  $ insert (exerciseId quadraticNoABCExercise)     quadraticNoABCExerciseInfo
  $ insert (exerciseId quadraticWithApproximation) quadraticWithApproximationExerciseInfo
  $ insert (exerciseId rationalEquationExercise)   rationalEquationExerciseInfo
  $ insert (exerciseId simplifyPowerExercise)      simplifyPowerExerciseInfo
  $ insert (exerciseId simplifyRationalExercise)   simplifyRationalExerciseInfo
    empty

calcPowerExerciseInfo :: MBExerciseInfo
calcPowerExerciseInfo = MBExerciseInfo
  { title             = "Calculating powers" {-
                        \l -> case l of
                          EN -> "Calculating powers"
                          ES -> "Calcular la potencia "
                          DE -> "Berechnen Sie die Potenz "
                          FR -> "Calculer la puissance "
                          NL -> "Machten berekenen"
                          FI -> "Laske arvo potenssilausekkeelle "
                      -}
  , for               = "mbase://mb_concepts/mb_numbers_and_computation/_01_02_05_03_Powers"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Calculate the power "
                          ES -> "Calcular la potencia "
                          DE -> "Berechnen Sie die Potenz "
                          FR -> "Calculer la puissance "
                          NL -> "Bereken de macht "
                          FI -> "Laske arvo potenssilausekkeelle "
  , problemStatement  = "Calculate the following power: "
  , context           = showId $ exerciseId calcPowerExercise
  , difficulty        = "medium"
  }

coverUpExerciseInfo :: MBExerciseInfo
coverUpExerciseInfo = MBExerciseInfo
  { title             = "Solving equations" {-
                        \l -> case l of
                          EN -> "Solving equations"
                          ES -> "Resolver la ecuación "
                          DE -> "Lösen Sie die Gleichung "
                          FR -> "Résoudre l'équation "
                          NL -> "Vergelijkingen oplossen"
                          FI -> "Ratkaise yhtälö "
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the equation "
                          ES -> "Resolver la ecuación "
                          DE -> "Lösen Sie die Gleichung "
                          FR -> "Résoudre l'équation "
                          NL -> "Los de volgende vergelijking op "
                          FI -> "Ratkaise yhtälö "
  , problemStatement  = "Solve the following equation: "
  , context           = showId $ exerciseId coverUpExercise
  , difficulty        = "medium"
  }

derivativeExerciseInfo :: MBExerciseInfo
derivativeExerciseInfo = MBExerciseInfo
  { title             = "Derivatives" {-
                        \l -> case l of
                          EN -> "Derivatives"
                          ES -> "Calcular la derivada de la función "
                          DE -> "Bestimmen Sie die Ableitung der Funktion "
                          FR -> "Calculer la dérivée de la fonction "
                          NL -> "Afgeleiden"
                          FI -> "Laske derivaatta funktiolle "
                      -}
  , for               = "mbase://mb_concepts/mb_calculus/_06_01_Single_Variable"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Calculate the following derivative of a function "
                          ES -> "Calcular la derivada de la función "
                          DE -> "Bestimmen Sie die Ableitung der Funktion "
                          FR -> "Calculer la dérivée de la fonction "
                          NL -> "Bereken de volgende afgeleide van een functie "
                          FI -> "Laske derivaatta funktiolle "
  , problemStatement  = "Calculate the derivative of the following function: "
  , context           = showId $ exerciseId derivativeExercise
  , difficulty        = "medium"
  }

derivativePolyExerciseInfo :: MBExerciseInfo
derivativePolyExerciseInfo = MBExerciseInfo
  { title             = "Differentiate polynomials" {-
                        \l -> case l of
                          EN -> "Differentiate polynomials"
                          ES -> "Calcular la derivada del polinomio "
                          DE -> "Bestimmen Sie die Ableitung des Polynoms "
                          FR -> "Calculer la dérivée du polynôme "
                          NL -> "Afgeleiden van polynomen"
                          FI -> "Laske derivaatta polynomille "
                      -}
  , for               = "mbase://mb_concepts/mb_calculus/_06_01_Single_Variable"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Calculate the following derivative of a polynomial "
                          ES -> "Calcular la derivada del polinomio "
                          DE -> "Bestimmen Sie die Ableitung des Polynoms "
                          FR -> "Calculer la dérivée du polynôme "
                          NL -> "Bereken de volgende afgeleide van een polynoom "
                          FI -> "Laske derivaatta polynomille "
  , problemStatement  = "Calculate the derivative of the following polynomial: "
  , context           = showId $ exerciseId derivativePolyExercise
  , difficulty        = "medium"
  }

derivativeProductExerciseInfo :: MBExerciseInfo
derivativeProductExerciseInfo = MBExerciseInfo
  { title             = "Differentiate product" {-
                        \l -> case l of
                          EN -> "Differentiate product"
                          ES -> "Calcular la derivada del producto "
                          DE -> "Bestimmen Sie die Ableitung des Produkts "
                          FR -> "Calculer la dérivée du produit "
                          NL -> "Afgeleiden van producten"
                          FI -> "Laske derivaatta tulolle "
                      -}
  , for               = "mbase://mb_concepts/mb_calculus/_06_01_Single_Variable"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Calculate the following derivative of a product "
                          ES -> "Calcular la derivada del producto "
                          DE -> "Bestimmen Sie die Ableitung des Produkts "
                          FR -> "Calculer la dérivée du produit "
                          NL -> "Bereken de volgende afgeleide van een product "
                          FI -> "Laske derivaatta tulolle "
  , problemStatement  = "Calculate the derivative of the following product: "
  , context           = showId $ exerciseId derivativeProductExercise
  , difficulty        = "medium"
  }

derivativeQuotientExerciseInfo :: MBExerciseInfo
derivativeQuotientExerciseInfo = MBExerciseInfo
  { title             = "Differentiate quotients" {-
                        \l -> case l of
                          EN -> "Differentiate quotients"
                          ES -> "Calcular la derivada del cociente "
                          DE -> "Bestimmen Sie die Ableitung des Quotienten "
                          FR -> "Calculer la dérivée du quotient "
                          NL -> "Afgeleiden van quotiënten"
                          FI -> "Laske derivaatta osamäärälle "
                      -}
  , for               = "mbase://mb_concepts/mb_calculus/_06_01_Single_Variable"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Calculate the following derivative of a quotient "
                          ES -> "Calcular la derivada del cociente "
                          DE -> "Bestimmen Sie die Ableitung des Quotienten "
                          FR -> "Calculer la dérivée du quotient "
                          NL -> "Bereken de volgende afgeleide van een quotiënt "
                          FI -> "Laske derivaatta osamäärälle "
  , problemStatement  = "Calculate the derivative of the following quotient: "
  , context           = showId $ exerciseId derivativeQuotientExercise
  , difficulty        = "medium"
  }

expEqExerciseInfo :: MBExerciseInfo
expEqExerciseInfo = MBExerciseInfo
  { title             = "Solving exponential equations" {-
                        \l -> case l of
                          EN -> "Solving exponential equations"
                          ES -> "Resolver la ecuación exponencial "
                          DE -> "Lösen Sie die Exponentialgleichung "
                          FR -> "Résoudre l'équation exponentielle "
                          NL -> "Exponentiële vergelijking oplossen"
                          FI -> "Ratkaise eksponenttiyhtälö "
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_05_Exponential_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the exponential equation "
                          ES -> "Resolver la ecuación exponencial "
                          DE -> "Lösen Sie die Exponentialgleichung "
                          FR -> "Résoudre l'équation exponentielle "
                          NL -> "Los de volgende exponentiële vergelijking op "
                          FI -> "Ratkaise eksponenttiyhtälö "
  , problemStatement  = "Solve the following exponential equation: "
  , context           = showId $ exerciseId expEqExercise
  , difficulty        = "medium"
  }

findFactorsExerciseInfo :: MBExerciseInfo
findFactorsExerciseInfo = MBExerciseInfo
  { title             = "Finding factors" {-
                        \l -> case l of
                          EN -> "Finding factors"
                          ES -> "Encontrar los factores "
                          DE -> "Finden Sie die Faktoren "
                          FR -> "Trouver les facteurs "
                          NL -> "Ontbinden in factoren"
                          FI -> "Ratkaise kertoimet "
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_02_Algebraic_Manipulation"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Find the factors "
                          ES -> "Encontrar los factores "
                          DE -> "Finden Sie die Faktoren "
                          FR -> "Trouver les facteurs "
                          NL -> "Ontbind in factoren "
                          FI -> "Ratkaise kertoimet "
  , problemStatement  = "Find the factors: "
  , context           = showId $ exerciseId findFactorsExercise
  , difficulty        = "easy"
  }

fractionExerciseInfo :: MBExerciseInfo
fractionExerciseInfo = MBExerciseInfo
  { title             = "Simplifying fractions" {-
                        \l -> case l of
                          EN -> "Simplifying fractions"
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Breuken vereenvoudigen"
                          FI -> "" 
                      -}
  , for               = "mbase://mb_concepts/mb_numbers_and_computation/_01_02_02_Fractions"
  , langSupported     = [EN]
  , cmp               = \l -> case l of 
                          EN -> "Simplify the fraction "
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Vereenvoudig de breuk "
                          FI -> "" 
  , problemStatement  = "Simplify the following fraction: "
  , context           = showId $ exerciseId fractionExercise
  , difficulty        = ""
  }

gaussianElimExerciseInfo :: MBExerciseInfo
gaussianElimExerciseInfo = MBExerciseInfo
  { title             = "Gaussian elimination" {-
                        \l -> case l of
                          EN -> "Gaussian elimination"
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Matrix vegen"
                          FI -> "" 
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_02_02_Matrix_Algebra"
  , langSupported     = [EN]
  , cmp               = \l -> case l of 
                          EN -> "Perform Gaussian elimination on "
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Veeg de matrix "
                          FI -> "" 
  , problemStatement  = "Perform Gaussian elimination: "
  , context           = showId $ exerciseId gaussianElimExercise
  , difficulty        = ""
  }

gramSchmidtExerciseInfo :: MBExerciseInfo
gramSchmidtExerciseInfo = MBExerciseInfo
  { title             = "Gram Schmidt" {-
                        \l -> case l of
                          EN -> "Gram Schmidt"
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Gram Schmidt"
                          FI -> "" 
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_02_04_Vector_Spaces"
  , langSupported     = [EN]
  , cmp               = \l -> case l of 
                          EN -> "Solve using Gram Schmidt "
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Los op mbv. Gram Schmidt"
                          FI -> "" 
  , problemStatement  = "Solve using Gram Schmidt: "
  , context           = showId $ exerciseId gramSchmidtExercise
  , difficulty        = ""
  }

higherDegreeExerciseInfo :: MBExerciseInfo
higherDegreeExerciseInfo = MBExerciseInfo
  { title             = "Solving higher degree polynomial equations" {-
                        \l -> case l of
                          EN -> "Solving higher degree polynomial equations"
                          ES -> "Resolver la ecuación de orden superior "
                          DE -> "Lösen Sie die Gleichung "
                          FR -> "Résoudre l'équation polynomiale de degré élevé "
                          NL -> "Hogeregraads polynoomvergelijkingen oplossen"
                          FI -> "Ratkaise korkeamman asteen polynomiyhätlö "
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_03_Polynomial_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the higher degree polynomial equation "
                          ES -> "Resolver la ecuación de orden superior "
                          DE -> "Lösen Sie die Gleichung "
                          FR -> "Résoudre l'équation polynomiale de degré élevé "
                          NL -> "Los de volgende hogeregraads polynoomvergelijking op "
                          FI -> "Ratkaise korkeamman asteen polynomiyhätlö "
  , problemStatement  = "Solve the following higher degree polynomial equation: "
  , context           = showId $ exerciseId higherDegreeExercise
  , difficulty        = "easy"
  }

ineqHigherDegreeExerciseInfo :: MBExerciseInfo
ineqHigherDegreeExerciseInfo = MBExerciseInfo
  { title             = "Solving inequations of higher degree" {-
                        \l -> case l of
                          EN -> "Solving inequations of higher degree"
                          ES -> "Resolver la inecuación "
                          DE -> "Lösen Sie die Ungleichung "
                          FR -> "Résoudre l'inéquation "
                          NL -> "Hogeregraads ongelijkheden oplossen "
                          FI -> "Ratkaise epäyhtälö "
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_05_Inequalities"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the inequation "
                          ES -> "Resolver la inecuación "
                          DE -> "Lösen Sie die Ungleichung "
                          FR -> "Résoudre l'inéquation "
                          NL -> "Los de volgende ongelijkheid op "
                          FI -> "Ratkaise epäyhtälö "
  , problemStatement  = "Solve the following inequation: "
  , context           = showId $ exerciseId ineqHigherDegreeExercise
  , difficulty        = "medium"
  }

ineqLinearExerciseInfo :: MBExerciseInfo
ineqLinearExerciseInfo = MBExerciseInfo
  { title             = "Solving linear inequations" {-
                        \l -> case l of
                          EN -> "Solving linear inequations"
                          ES -> "Resolver la inecuación linea "
                          DE -> "Lösen Sie die lineare Ungleichung "
                          FR -> "Résoudre l'inéquation linéaire "
                          NL -> "Lineaire ongelijkheden oplossen"
                          FI -> "Ratkaise lineaarinen epäyhtälö "
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_05_Inequalities"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the linear inequation "
                          ES -> "Resolver la inecuación linea "
                          DE -> "Lösen Sie die lineare Ungleichung "
                          FR -> "Résoudre l'inéquation linéaire "
                          NL -> "Los de volgende lineaire ongelijkheid op "
                          FI -> "Ratkaise lineaarinen epäyhtälö "
  , problemStatement  = "Solve the following linear inequation: "
  , context           = showId $ exerciseId ineqLinearExercise
  , difficulty        = "medium"
  }

ineqQuadraticExerciseInfo :: MBExerciseInfo
ineqQuadraticExerciseInfo = MBExerciseInfo
  { title             = "Solving quadratic inequations" {-
                        \l -> case l of
                          EN -> "Solving quadratic inequations"
                          ES -> "Resolver la inecuación "
                          DE -> "Lösen Sie die Ungleichung "
                          FR -> "Résoudre l'inéquation "
                          NL -> "Kwadratische ongelijkheden oplossen"
                          FI -> "Ratkaise epäyhtälö "
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_05_Inequalities"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the inequation "
                          ES -> "Resolver la inecuación "
                          DE -> "Lösen Sie die Ungleichung "
                          FR -> "Résoudre l'inéquation "
                          NL -> "Los de volgende ongelijkheid op "
                          FI -> "Ratkaise epäyhtälö "
  , problemStatement  = "Solve the following inequation: "
  , context           = showId $ exerciseId ineqQuadraticExercise
  , difficulty        = "medium"
  }

linearExerciseInfo :: MBExerciseInfo
linearExerciseInfo = MBExerciseInfo
  { title             = "Solving linear equations" {-
                        \l -> case l of
                          EN -> "Solving linear equations"
                          ES -> "Resolver la ecuación lineal "
                          DE -> "Lösen Sie die lineare Gleichung "
                          FR -> "Résoudre l'équation linéaire "
                          NL -> "Lineaire vergelijkingen oplossen"
                          FI -> "Ratkaise lineaarinen yhtälö "
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_01_Linear_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the linear equation "
                          ES -> "Resolver la ecuación lineal "
                          DE -> "Lösen Sie die lineare Gleichung "
                          FR -> "Résoudre l'équation linéaire "
                          NL -> "Los de volgende lineaire vergelijking op "
                          FI -> "Ratkaise lineaarinen yhtälö "
  , problemStatement  = "Solve the following equation: "
  , context           = showId $ exerciseId linearExercise
  , difficulty        = "easy"
  }

linearMixedExerciseInfo :: MBExerciseInfo
linearMixedExerciseInfo = MBExerciseInfo
  { title             = "Solving linear mixed equations" {-
                        \l -> case l of
                          EN -> "Solving linear mixed equations"
                          ES -> "Resolver la ecuación lineal compuesta "
                          DE -> "Lösen Sie die lineare Gleichung "
                          FR -> "Résoudre l'équation linéaire mixte "
                          NL -> "Gemengde lineaire vergelijkingen oplossen"
                          FI -> "Ratkaise lineaarinen yhtälö "
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_01_Linear_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the linear mixed equation "
                          ES -> "Resolver la ecuación lineal compuesta "
                          DE -> "Lösen Sie die lineare Gleichung "
                          FR -> "Résoudre l'équation linéaire mixte "
                          NL -> "Los de volgende gemengde lineaire vergelijking op "
                          FI -> "Ratkaise lineaarinen yhtälö "
  , problemStatement  = "Solve the linear mixed equation: "
  , context           = showId $ exerciseId linearMixedExercise
  , difficulty        = "easy"
  }

linearSystemExerciseInfo :: MBExerciseInfo
linearSystemExerciseInfo = MBExerciseInfo
  { title             = "Solving systems of linear equations" {-
                        \l -> case l of
                          EN -> "Solving systems of linear equations"
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Systemen van lineaire vergelijkingen oplossen"
                          FI -> "" 
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_07_Systems_of_Equations"
  , langSupported     = [EN]
  , cmp               = \l -> case l of 
                          EN -> "Solve the system of linear equations "
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Los het volgende systeem van lineaire vergelijkingen op"
                          FI -> "" 
  , problemStatement  = "Solve the following system of linear equations: "
  , context           = showId $ exerciseId linearSystemExercise
  , difficulty        = ""
  }

logEqExerciseInfo :: MBExerciseInfo
logEqExerciseInfo = MBExerciseInfo
  { title             = "Solving logarithmic equations" {-
                        \l -> case l of
                          EN -> "Solving logarithmic equations"
                          ES -> "Resolver la ecuación logarítmica "
                          DE -> "Lösen Sie die Gleichung mit Logarithmen "
                          FR -> "Résoudre l'équation logarithmique "
                          NL -> "Logarithmische vergelijkingen oplossen"
                          FI -> "Ratkaise logaritmiyhtälö "
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_06_Logarithmic_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the logarithmic equation "
                          ES -> "Resolver la ecuación logarítmica "
                          DE -> "Lösen Sie die Gleichung mit Logarithmen "
                          FR -> "Résoudre l'équation logarithmique "
                          NL -> "Los de volgende logaritmische vergelijking op "
                          FI -> "Ratkaise logaritmiyhtälö "
  , problemStatement  = "Solve the following logarithmic equation: "
  , context           = showId $ exerciseId logEqExercise
  , difficulty        = "medium"
  }

nonNegBrokenExpExerciseInfo :: MBExerciseInfo
nonNegBrokenExpExerciseInfo = MBExerciseInfo
  { title             = "Writing with non-negative exponents" {-
                        \l -> case l of
                          EN -> "Writing with non-negative exponents"
                          ES -> "Expresar con exponente no negativo "
                          DE -> "Schreiben Sie mit nicht-negativen Exponenten "
                          FR -> "Écrire avec une puissance non-négative "
                          NL -> "Met niet-negatieve exponenten schrijven"
                          FI -> "Ilmaise käyttäen ei-negatiivista eksponenttia "
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_02_Algebraic_Manipulation"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Write with a non-negative exponent "
                          ES -> "Expresar con exponente no negativo "
                          DE -> "Schreiben Sie mit nicht-negativen Exponenten "
                          FR -> "Écrire avec une puissance non-négative <"
                          NL -> "Schrijf met niet-negatieve exponenten "
                          FI -> "Ilmaise käyttäen ei-negatiivista eksponenttia "
  , problemStatement  = "Write the following with a non-negative exponent: "
  , context           = showId $ exerciseId nonNegBrokenExpExercise
  , difficulty        = "medium"
  }

powerEqExerciseInfo :: MBExerciseInfo
powerEqExerciseInfo = MBExerciseInfo
  { title             = "Solving power equations" {-
                        \l -> case l of
                          EN -> "Solving power equations"
                          ES -> "Resolver la ecuación con potencias "
                          DE -> "Lösen Sie die Gleichung mit x im Exponenten "
                          FR -> "Résoudre l'équation avec puissances "
                          NL -> "Machtsvergelijkingen oplossen"
                          FI -> "Ratkaise potenssiyhtälö "
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_03_Polynomial_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the power equation "
                          ES -> "Resolver la ecuación con potencias "
                          DE -> "Lösen Sie die Gleichung mit x im Exponenten "
                          FR -> "Résoudre l'équation avec puissances "
                          NL -> "Los de volgende machtsvergelijking op "
                          FI -> "Ratkaise potenssiyhtälö "
  , problemStatement  = "Solve the following power equation: "
  , context           = showId $ exerciseId powerEqExercise
  , difficulty        = "medium"
  }

powerOfExerciseInfo :: MBExerciseInfo
powerOfExerciseInfo = MBExerciseInfo
  { title             = "Writing as a power" {-
                        \l -> case l of
                          EN -> "Writing as a power"
                          ES -> "Expresar como potencia "
                          DE -> "Schreiben Sie als Potenz "
                          FR -> "Écrire sous la forme d'une puissance "
                          NL -> "Als een macht schrijven"
                          FI -> "Ilmaise potenssimuodossa "
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_02_Algebraic_Manipulation"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Write as a power "
                          ES -> "Expresar como potencia "
                          DE -> "Schreiben Sie als Potenz "
                          FR -> "Écrire sous la forme d'une puissance "
                          NL -> "Schrijf als een macht "
                          FI -> "Ilmaise potenssimuodossa "
  , problemStatement  = "Write the following as a power: "
  , context           = showId $ exerciseId powerOfExercise
  , difficulty        = "medium"
  }

quadraticExerciseInfo :: MBExerciseInfo
quadraticExerciseInfo = MBExerciseInfo
  { title             = "Solving quadratic equations" {-
                        \l -> case l of
                          EN -> "Solving quadratic equations"
                          ES -> "Resolver la ecuación cuadrática "
                          DE -> "Lösen Sie die quadratische Gleichung "
                          FR -> "Résoudre l'équation quadratique "
                          NL -> "Kwadratische vergelijkingen oplossen"
                          FI -> "Ratkaise toisen asteen yhtälö "
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_02_Quadratic_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the quadratic equation "
                          ES -> "Resolver la ecuación cuadrática "
                          DE -> "Lösen Sie die quadratische Gleichung "
                          FR -> "Résoudre l'équation quadratique "
                          NL -> "Los de volgende kwadratische vergelijking op "
                          FI -> "Ratkaise toisen asteen yhtälö "
  , problemStatement  = "Solve the following quadratic equation: "
  , context           = showId $ exerciseId quadraticExercise
  , difficulty        = "easy"
  }
  
quadraticNoABCExerciseInfo :: MBExerciseInfo
quadraticNoABCExerciseInfo = MBExerciseInfo
  { title             = "Solving quadratic equations (no abc)" {-
                        \l -> case l of
                          EN -> "Solving quadratic equations (no abc)"
                          ES -> "Resolver la ecuación cuadrática sin utilizar la fórmula cuadrática "
                          DE -> "Lösen Sie die quadratische Gleichung ohne Benutzung der p-q-Formel "
                          FR -> "Résoudre l'équation quadratique, sans utiliser la formule de résolution "
                          NL -> "Kwadratische vergelijkingen oplossen (geen abc)"
                          FI -> "Ratkaise toisen asteen yhtälö käyttämättä ratkaisukaavaa "
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_02_Quadratic_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve, without using the quadratic formula, the quadratic equation "
                          ES -> "Resolver la ecuación cuadrática sin utilizar la fórmula cuadrática "
                          DE -> "Lösen Sie die quadratische Gleichung ohne Benutzung der p-q-Formel "
                          FR -> "Résoudre l'équation quadratique, sans utiliser la formule de résolution "
                          NL -> "Los de volgende kwadratische vergelijking op, zonder gebruik te maken van de abc-formule "
                          FI -> "Ratkaise toisen asteen yhtälö käyttämättä ratkaisukaavaa "
  , problemStatement  = "Solve, without using the quadratic formula, the following quadratic equation: "
  , context           = showId $ exerciseId quadraticNoABCExercise
  , difficulty        = "easy"
  }
  
quadraticWithApproximationExerciseInfo :: MBExerciseInfo
quadraticWithApproximationExerciseInfo = MBExerciseInfo
  { title             = "Solving quadratic equations (with approximation)" {-
                        \l -> case l of
                          EN -> "Solving quadratic equations (with approximation)"
                          ES -> "Resolver la ecuación cuadrática, con aproximación permitida "
                          DE -> "Lösen Sie die quadratische Gleichung"
                          FR -> "Résoudre l'équation quadratique, approximation permise "
                          NL -> "Kwadratische vergelijkingen oplossen (met benaderingen)"
                          FI -> "Ratkaise likimääräisesti toisen asteen yhtälö "
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_02_Quadratic_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve, with approximation allowed, the quadratic equation "
                          ES -> "Resolver la ecuación cuadrática, con aproximación permitida "
                          DE -> "Lösen Sie die quadratische Gleichung"
                          FR -> "Résoudre l'équation quadratique, approximation permise "
                          NL -> "Los de volgende kwadratische vergelijking op, een benadering is toegestaan "
                          FI -> "Ratkaise likimääräisesti toisen asteen yhtälö "
  , problemStatement  = "Solve, with approximation allowed, the following quadratic equation: "
  , context           = showId $ exerciseId quadraticWithApproximation
  , difficulty        = "easy"
  }

rationalEquationExerciseInfo :: MBExerciseInfo
rationalEquationExerciseInfo = MBExerciseInfo
  { title             = "Solving rational equations" {-
                        \l -> case l of
                          EN -> "Solving rational equations"
                          ES -> "Resolver la ecuación racional "
                          DE -> "Lösen Sie die Bruchgleichung "
                          FR -> "Résoudre l'équation rationnelle "
                          NL -> "Gebroken vergelijkingen oplossen"
                          FI -> "Ratkaise rationaaliyhtälö "
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_04_Rational_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the rational equation "
                          ES -> "Resolver la ecuación racional "
                          DE -> "Lösen Sie die Bruchgleichung "
                          FR -> "Résoudre l'équation rationnelle "
                          NL -> "Los de volgende gebroken vergelijking op "
                          FI -> "Ratkaise rationaaliyhtälö "
  , problemStatement  = "Solve the following rational equation: "
  , context           = showId $ exerciseId rationalEquationExercise
  , difficulty        = "medium"
  }

simplifyPowerExerciseInfo :: MBExerciseInfo
simplifyPowerExerciseInfo = MBExerciseInfo
  { title             = "Simplifying powers" {-
                        \l -> case l of
                          EN -> "Simplifying powers"
                          ES -> "Simplificar las potencias "
                          DE -> "Vereinfachen Sie die Potenz "
                          FR -> "Simplifier la puissance "
                          NL -> "Machten vereenvoudigen"
                          FI -> "Sievennä potenssilauseke "
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_02_Algebraic_Manipulation"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Simplify the power "
                          ES -> "Simplificar las potencias "
                          DE -> "Vereinfachen Sie die Potenz "
                          FR -> "Simplifier la puissance "
                          NL -> "Vereenvoudig de macht "
                          FI -> "Sievennä potenssilauseke "
  , problemStatement  = "Simplify the following power: "
  , context           = showId $ exerciseId simplifyPowerExercise
  , difficulty        = "medium"
  }

simplifyRationalExerciseInfo :: MBExerciseInfo
simplifyRationalExerciseInfo = MBExerciseInfo
  { title             = "Simplifying rationals" {-
                        \l -> case l of
                          EN -> "Simplifying rationals"
                          ES -> "Simplificar el racional "
                          DE -> "Vereinfachen Sie den Bruch "
                          FR -> "Simplifiez le rationnel "
                          NL -> "Breuken vereenvoudigen"
                          FI -> "Sievennä murtolauseke "
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_02_Algebraic_Manipulation"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Simplify the rational "
                          ES -> "Simplificar el racional "
                          DE -> "Vereinfachen Sie den Bruch "
                          FR -> "Simplifiez le rationnel "
                          NL -> "Vereenvoudig de breuk "
                          FI -> "Sievennä murtolauseke "
  , problemStatement  = "Simplify the following rational: "
  , context           = showId $ exerciseId simplifyRationalExercise
  , difficulty        = "medium"
  }

systemWithMatrixExerciseInfo :: MBExerciseInfo
systemWithMatrixExerciseInfo = MBExerciseInfo
  { title             = "Solving systems of linear equations using matrices" {-
                        \l -> case l of
                          EN -> "Solving systems of linear equations using matrices"
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Systemen van lineaire vergelijkingen oplossen mbv. matrices"
                          FI -> "" 
                      -}
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_02_02_Matrix_Algebra"
  , langSupported     = [EN]
  , cmp               = \l -> case l of 
                          EN -> "Solve the following system of linear equations using a matrix "
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Los het volgende systeem van lineaire vergelijkingen op mbv. matrices "
                          FI -> "" 
  , problemStatement  = "Solve the following system of linear equations using a matrix:  "
  , context           = showId $ exerciseId systemWithMatrixExercise
  , difficulty        = ""
  }
