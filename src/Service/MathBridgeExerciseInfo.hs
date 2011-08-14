module Service.MathBridgeExerciseInfo where

-- Derivative formulation corrected: EN, NL, FI
-- Title translated: EN, NL, FI

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

data Lang = EN | ES | DE | FR | NL | FI | HU | ALL 

instance Show Lang where
  show EN = "en"
  show ES = "es"
  show DE = "de"
  show FR = "fr"
  show NL = "nl"
  show FI = "fi"
  show HU = "hu"

--------------------------------------------------------------------------------
{- Info about exercises for ActiveMath
-}
--------------------------------------------------------------------------------

data MBExerciseInfo = MBExerciseInfo
  { title             :: Lang -> String
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
  { title             = \l -> case l of
                          EN -> "Calculating powers"
                          ES -> "Calcular la potencia "
                          DE -> "Berechnen Sie die Potenz "
                          FR -> "Calculer avec des puissances "
                          NL -> "Machten berekenen"
                          FI -> "Potenssien laskeminen "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_numbers_and_computation/_01_02_05_03_Powers"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Calculate the power "
                          ES -> "Calcular la potencia "
                          DE -> "Berechnen Sie die Potenz "
                          FR -> "Calculer la puissance "
                          NL -> "Bereken de macht "
                          FI -> "Laske arvo potenssilausekkeelle "
                          HU -> ""
  , problemStatement  = "Calculate the following power: "
  , context           = showId $ exerciseId calcPowerExercise
  , difficulty        = "medium"
  }

coverUpExerciseInfo :: MBExerciseInfo
coverUpExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving equations"
                          ES -> "Resolver la ecuación "
                          DE -> "Lösen Sie die Gleichung "
                          FR -> "Résoudre des équations "
                          NL -> "Vergelijkingen oplossen"
                          FI -> "Yhtälöiden ratkaiseminen "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the equation "
                          ES -> "Resolver la ecuación "
                          DE -> "Lösen Sie die Gleichung "
                          FR -> "Résoudre l'équation "
                          NL -> "Los de volgende vergelijking op "
                          FI -> "Ratkaise yhtälö "
                          HU -> ""
  , problemStatement  = "Solve the following equation: "
  , context           = showId $ exerciseId coverUpExercise
  , difficulty        = "medium"
  }

derivativeExerciseInfo :: MBExerciseInfo
derivativeExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Derivatives"
                          ES -> "Calcular la derivada de la función "
                          DE -> "Bestimmen Sie die Ableitung der Funktion "
                          FR -> "Dérivées "
                          NL -> "Afgeleiden"
                          FI -> "Derivaatat "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_calculus/_06_01_04_Differentiation"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Calculate the following derivative of a function "
                          ES -> "Calcular la derivada de la función "
                          DE -> "Bestimmen Sie die Ableitung der Funktion "
                          FR -> "Calculer la dérivée de la fonction "
                          NL -> "Bereken de volgende afgeleide van een functie "
                          FI -> "Laske seuraava derivaatta "
                          HU -> ""
  , problemStatement  = "Calculate the derivative of the following function: "
  , context           = showId $ exerciseId derivativeExercise
  , difficulty        = "medium"
  }

derivativePolyExerciseInfo :: MBExerciseInfo
derivativePolyExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Differentiate polynomials"
                          ES -> "Calcular la derivada del polinomio "
                          DE -> "Bestimmen Sie die Ableitung des Polynoms "
                          FR -> "Dérivée d'un polynôme "
                          NL -> "Afgeleiden van polynomen"
                          FI -> "Derivaatan laskeminen polynomille "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_calculus/_06_01_04_Differentiation"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Calculate the following derivative of a polynomial "
                          ES -> "Calcular la derivada del polinomio "
                          DE -> "Bestimmen Sie die Ableitung des Polynoms "
                          FR -> "Calculer la dérivée du polynôme "
                          NL -> "Bereken de volgende afgeleide van een polynoom "
                          FI -> "Laske seuraava derivaatta "
                          HU -> ""
  , problemStatement  = "Calculate the derivative of the following polynomial: "
  , context           = showId $ exerciseId derivativePolyExercise
  , difficulty        = "medium"
  }

derivativeProductExerciseInfo :: MBExerciseInfo
derivativeProductExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Differentiate products"
                          ES -> "Calcular la derivada del producto "
                          DE -> "Bestimmen Sie die Ableitung des Produkts "
                          FR -> "Dérivée d'un produit "
                          NL -> "Afgeleiden van producten"
                          FI -> "Derivaatan laskeminen tulolle "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_calculus/_06_01_04_Differentiation"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Calculate the following derivative of a product "
                          ES -> "Calcular la derivada del producto "
                          DE -> "Bestimmen Sie die Ableitung des Produkts "
                          FR -> "Calculer la dérivée du produit "
                          NL -> "Bereken de volgende afgeleide van een product "
                          FI -> "Laske seuraava derivaatta "
                          HU -> ""
  , problemStatement  = "Calculate the derivative of the following product: "
  , context           = showId $ exerciseId derivativeProductExercise
  , difficulty        = "medium"
  }

derivativeQuotientExerciseInfo :: MBExerciseInfo
derivativeQuotientExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Differentiate quotients"
                          ES -> "Calcular la derivada del cociente "
                          DE -> "Bestimmen Sie die Ableitung des Quotienten "
                          FR -> "Dérivée d'un quotient "
                          NL -> "Afgeleiden van quotiënten"
                          FI -> "Derivaatan laskeminen osamäärälle "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_calculus/_06_01_04_Differentiation"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Calculate the following derivative of a quotient "
                          ES -> "Calcular la derivada del cociente "
                          DE -> "Bestimmen Sie die Ableitung des Quotienten "
                          FR -> "Calculer la dérivée du quotient "
                          NL -> "Bereken de volgende afgeleide van een quotiënt "
                          FI -> "Laske derivaatta osamäärälle "
                          HU -> ""
  , problemStatement  = "Calculate the derivative of the following quotient: "
  , context           = showId $ exerciseId derivativeQuotientExercise
  , difficulty        = "medium"
  }

expEqExerciseInfo :: MBExerciseInfo
expEqExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving exponential equations"
                          ES -> "Resolver la ecuación exponencial "
                          DE -> "Lösen Sie die Exponentialgleichung "
                          FR -> "Équation exponentielle "
                          NL -> "Exponentiële vergelijking oplossen"
                          FI -> "Eksponenttiyhtälöiden ratkaiseminen "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_05_Exponential_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the exponential equation "
                          ES -> "Resolver la ecuación exponencial "
                          DE -> "Lösen Sie die Exponentialgleichung "
                          FR -> "Résoudre l'équation exponentielle "
                          NL -> "Los de volgende exponentiële vergelijking op "
                          FI -> "Ratkaise eksponenttiyhtälö "
                          HU -> ""
  , problemStatement  = "Solve the following exponential equation: "
  , context           = showId $ exerciseId expEqExercise
  , difficulty        = "medium"
  }

findFactorsExerciseInfo :: MBExerciseInfo
findFactorsExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Finding factors"
                          ES -> "Encontrar los factores "
                          DE -> "Finden Sie die Faktoren "
                          FR -> "Trouver des facteurs "
                          NL -> "Ontbinden in factoren"
                          FI -> "Kertoimien ratkaiseminen "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_02_Algebraic_Manipulation"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Find the factors "
                          ES -> "Encontrar los factores "
                          DE -> "Finden Sie die Faktoren "
                          FR -> "Trouver les facteurs "
                          NL -> "Ontbind in factoren "
                          FI -> "Ratkaise kertoimet "
                          HU -> ""
  , problemStatement  = "Find the factors: "
  , context           = showId $ exerciseId findFactorsExercise
  , difficulty        = "easy"
  }

fractionExerciseInfo :: MBExerciseInfo
fractionExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Simplifying fractions"
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Breuken vereenvoudigen"
                          FI -> "" 
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_numbers_and_computation/_01_02_02_Fractions"
  , langSupported     = [EN]
  , cmp               = \l -> case l of 
                          EN -> "Simplify the fraction "
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Vereenvoudig de breuk "
                          FI -> "" 
                          HU -> ""
  , problemStatement  = "Simplify the following fraction: "
  , context           = showId $ exerciseId fractionExercise
  , difficulty        = ""
  }

gaussianElimExerciseInfo :: MBExerciseInfo
gaussianElimExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Gaussian elimination"
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Matrix vegen"
                          FI -> "" 
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_02_02_Matrix_Algebra"
  , langSupported     = [EN]
  , cmp               = \l -> case l of 
                          EN -> "Perform Gaussian elimination on "
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Veeg de matrix "
                          FI -> "" 
                          HU -> ""
  , problemStatement  = "Perform Gaussian elimination: "
  , context           = showId $ exerciseId gaussianElimExercise
  , difficulty        = ""
  }

gramSchmidtExerciseInfo :: MBExerciseInfo
gramSchmidtExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Gram Schmidt"
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Gram Schmidt"
                          FI -> "" 
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_02_04_Vector_Spaces"
  , langSupported     = [EN]
  , cmp               = \l -> case l of 
                          EN -> "Solve using Gram Schmidt "
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Los op mbv. Gram Schmidt"
                          FI -> "" 
                          HU -> ""
  , problemStatement  = "Solve using Gram Schmidt: "
  , context           = showId $ exerciseId gramSchmidtExercise
  , difficulty        = ""
  }

higherDegreeExerciseInfo :: MBExerciseInfo
higherDegreeExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving higher degree polynomial equations"
                          ES -> "Resolver la ecuación de orden superior "
                          DE -> "Lösen Sie die Gleichung "
                          FR -> "Équation polynomiale "
                          NL -> "Hogeregraads polynoomvergelijkingen oplossen"
                          FI -> "Korkeamman asteen polynomiyhtälön ratkaiseminen "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_03_Polynomial_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the higher degree polynomial equation "
                          ES -> "Resolver la ecuación de orden superior "
                          DE -> "Lösen Sie die Gleichung "
                          FR -> "Résoudre l'équation polynomiale de degré élevé "
                          NL -> "Los de volgende hogeregraads polynoomvergelijking op "
                          FI -> "Ratkaise korkeamman asteen polynomiyhätlö "
                          HU -> ""
  , problemStatement  = "Solve the following higher degree polynomial equation: "
  , context           = showId $ exerciseId higherDegreeExercise
  , difficulty        = "easy"
  }

ineqHigherDegreeExerciseInfo :: MBExerciseInfo
ineqHigherDegreeExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving inequations of higher degree"
                          ES -> "Resolver la inecuación "
                          DE -> "Lösen Sie die Ungleichung "
                          FR -> "Inéquation de degré supérieur "
                          NL -> "Hogeregraads ongelijkheden oplossen "
                          FI -> "Korkeamman asteen epäyhtälöiden ratkaiseminen "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_05_Inequalities"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the inequation "
                          ES -> "Resolver la inecuación "
                          DE -> "Lösen Sie die Ungleichung "
                          FR -> "Résoudre l'inéquation "
                          NL -> "Los de volgende ongelijkheid op "
                          FI -> "Ratkaise epäyhtälö "
                          HU -> ""
  , problemStatement  = "Solve the following inequation: "
  , context           = showId $ exerciseId ineqHigherDegreeExercise
  , difficulty        = "medium"
  }

ineqLinearExerciseInfo :: MBExerciseInfo
ineqLinearExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving linear inequations"
                          ES -> "Resolver la inecuación linea "
                          DE -> "Lösen Sie die lineare Ungleichung "
                          FR -> "Inéquations linéaires "
                          NL -> "Lineaire ongelijkheden oplossen"
                          FI -> "Lineaaristen epäyhtälöiden ratkaiseminen "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_05_Inequalities"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the linear inequation "
                          ES -> "Resolver la inecuación linea "
                          DE -> "Lösen Sie die lineare Ungleichung "
                          FR -> "Résoudre l'inéquation linéaire "
                          NL -> "Los de volgende lineaire ongelijkheid op "
                          FI -> "Ratkaise lineaarinen epäyhtälö "
                          HU -> ""
  , problemStatement  = "Solve the following linear inequation: "
  , context           = showId $ exerciseId ineqLinearExercise
  , difficulty        = "medium"
  }

ineqQuadraticExerciseInfo :: MBExerciseInfo
ineqQuadraticExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving quadratic inequations"
                          ES -> "Resolver la inecuación "
                          DE -> "Lösen Sie die Ungleichung "
                          FR -> "Inéquation du second degré "
                          NL -> "Kwadratische ongelijkheden oplossen"
                          FI -> "Kvadraattisien epäyhtälöiden ratkaiseminen "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_05_Inequalities"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the inequation "
                          ES -> "Resolver la inecuación "
                          DE -> "Lösen Sie die Ungleichung "
                          FR -> "Résoudre l'inéquation "
                          NL -> "Los de volgende ongelijkheid op "
                          FI -> "Ratkaise epäyhtälö "
                          HU -> ""
  , problemStatement  = "Solve the following inequation: "
  , context           = showId $ exerciseId ineqQuadraticExercise
  , difficulty        = "medium"
  }

linearExerciseInfo :: MBExerciseInfo
linearExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving linear equations"
                          ES -> "Resolver la ecuación lineal "
                          DE -> "Lösen Sie die lineare Gleichung "
                          FR -> "Équation linéaire "
                          NL -> "Lineaire vergelijkingen oplossen"
                          FI -> "Lineaaristen yhtälöiden ratkaiseminen "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_01_Linear_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the linear equation "
                          ES -> "Resolver la ecuación lineal "
                          DE -> "Lösen Sie die lineare Gleichung "
                          FR -> "Résoudre l'équation linéaire "
                          NL -> "Los de volgende lineaire vergelijking op "
                          FI -> "Ratkaise lineaarinen yhtälö "
                          HU -> ""
  , problemStatement  = "Solve the following equation: "
  , context           = showId $ exerciseId linearExercise
  , difficulty        = "easy"
  }

linearMixedExerciseInfo :: MBExerciseInfo
linearMixedExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving linear mixed equations"
                          ES -> "Resolver la ecuación lineal compuesta "
                          DE -> "Lösen Sie die lineare Gleichung "
                          FR -> "Équation linéaire mixte "
                          NL -> "Gemengde lineaire vergelijkingen oplossen"
                          FI -> "Lineaaristen sekayhtälöiden ratkaisu "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_01_Linear_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the linear mixed equation "
                          ES -> "Resolver la ecuación lineal compuesta "
                          DE -> "Lösen Sie die lineare Gleichung "
                          FR -> "Résoudre l'équation linéaire mixte "
                          NL -> "Los de volgende gemengde lineaire vergelijking op "
                          FI -> "Ratkaise lineaarinen yhtälö "
                          HU -> ""
  , problemStatement  = "Solve the linear mixed equation: "
  , context           = showId $ exerciseId linearMixedExercise
  , difficulty        = "easy"
  }

linearSystemExerciseInfo :: MBExerciseInfo
linearSystemExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving systems of linear equations"
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Systemen van lineaire vergelijkingen oplossen"
                          FI -> "" 
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_07_Systems_of_Equations"
  , langSupported     = [EN]
  , cmp               = \l -> case l of 
                          EN -> "Solve the system of linear equations "
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Los het volgende systeem van lineaire vergelijkingen op"
                          FI -> "" 
                          HU -> ""
  , problemStatement  = "Solve the following system of linear equations: "
  , context           = showId $ exerciseId linearSystemExercise
  , difficulty        = ""
  }

logEqExerciseInfo :: MBExerciseInfo
logEqExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving logarithmic equations"
                          ES -> "Resolver la ecuación logarítmica "
                          DE -> "Lösen Sie die Gleichung mit Logarithmen "
                          FR -> "Équation logarithmique "
                          NL -> "Logarithmische vergelijkingen oplossen"
                          FI -> "Logaritmiyhtälöiden ratkaisu "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_06_Logarithmic_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the logarithmic equation "
                          ES -> "Resolver la ecuación logarítmica "
                          DE -> "Lösen Sie die Gleichung mit Logarithmen "
                          FR -> "Résoudre l'équation logarithmique "
                          NL -> "Los de volgende logaritmische vergelijking op "
                          FI -> "Ratkaise logaritmiyhtälö "
                          HU -> ""
  , problemStatement  = "Solve the following logarithmic equation: "
  , context           = showId $ exerciseId logEqExercise
  , difficulty        = "medium"
  }

nonNegBrokenExpExerciseInfo :: MBExerciseInfo
nonNegBrokenExpExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Writing with non-negative exponents"
                          ES -> "Expresar con exponente no negativo "
                          DE -> "Schreiben Sie mit nicht-negativen Exponenten "
                          FR -> "Écrire avec une puissance positive "
                          NL -> "Met niet-negatieve exponenten schrijven"
                          FI -> "Ilmaiseminen käyttäen ei-negatiivista eksponenttia "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_02_Algebraic_Manipulation"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Write with a non-negative exponent "
                          ES -> "Expresar con exponente no negativo "
                          DE -> "Schreiben Sie mit nicht-negativen Exponenten "
                          FR -> "Écrire avec une puissance non-négative <"
                          NL -> "Schrijf met niet-negatieve exponenten "
                          FI -> "Ilmaise käyttäen ei-negatiivista eksponenttia "
                          HU -> ""
  , problemStatement  = "Write the following with a non-negative exponent: "
  , context           = showId $ exerciseId nonNegBrokenExpExercise
  , difficulty        = "medium"
  }

powerEqExerciseInfo :: MBExerciseInfo
powerEqExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving power equations"
                          ES -> "Resolver la ecuación con potencias "
                          DE -> "Lösen Sie die Gleichung mit x im Exponenten "
                          FR -> "Équation avec puissances "
                          NL -> "Machtsvergelijkingen oplossen"
                          FI -> "Potenssiyhtälöiden ratkaiseminen "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_03_Polynomial_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the power equation "
                          ES -> "Resolver la ecuación con potencias "
                          DE -> "Lösen Sie die Gleichung mit x im Exponenten "
                          FR -> "Résoudre l'équation avec puissances "
                          NL -> "Los de volgende machtsvergelijking op "
                          FI -> "Ratkaise potenssiyhtälö "
                          HU -> ""
  , problemStatement  = "Solve the following power equation: "
  , context           = showId $ exerciseId powerEqExercise
  , difficulty        = "medium"
  }

powerOfExerciseInfo :: MBExerciseInfo
powerOfExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Writing as a power"
                          ES -> "Expresar como potencia "
                          DE -> "Schreiben Sie als Potenz "
                          FR -> "Écrire sous la forme d'une puissance "
                          NL -> "Als een macht schrijven"
                          FI -> "Ilmaiseminen potenssimuodossa "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_02_Algebraic_Manipulation"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Write as a power "
                          ES -> "Expresar como potencia "
                          DE -> "Schreiben Sie als Potenz "
                          FR -> "Écrire sous la forme d'une puissance "
                          NL -> "Schrijf als een macht "
                          FI -> "Ilmaise potenssimuodossa "
                          HU -> ""
  , problemStatement  = "Write the following as a power: "
  , context           = showId $ exerciseId powerOfExercise
  , difficulty        = "medium"
  }

quadraticExerciseInfo :: MBExerciseInfo
quadraticExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving quadratic equations"
                          ES -> "Resolver la ecuación cuadrática "
                          DE -> "Lösen Sie die quadratische Gleichung "
                          FR -> "Équation du second degré "
                          NL -> "Kwadratische vergelijkingen oplossen"
                          FI -> "Toisen asteen yhtälöiden ratkaiseminen "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_02_Quadratic_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the quadratic equation "
                          ES -> "Resolver la ecuación cuadrática "
                          DE -> "Lösen Sie die quadratische Gleichung "
                          FR -> "Résoudre l'équation quadratique "
                          NL -> "Los de volgende kwadratische vergelijking op "
                          FI -> "Ratkaise toisen asteen yhtälö "
                          HU -> ""
  , problemStatement  = "Solve the following quadratic equation: "
  , context           = showId $ exerciseId quadraticExercise
  , difficulty        = "easy"
  }
  
quadraticNoABCExerciseInfo :: MBExerciseInfo
quadraticNoABCExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving quadratic equations (no abc)"
                          ES -> "Resolver la ecuación cuadrática sin utilizar la fórmula cuadrática "
                          DE -> "Lösen Sie die quadratische Gleichung ohne Benutzung der p-q-Formel "
                          FR -> "Résoudre une équation quadratique, sans utiliser la formule de résolution "
                          NL -> "Kwadratische vergelijkingen oplossen (geen abc)"
                          FI -> "Toisen asteen yhtälöiden ratkaiseminen käyttämättä ratkaisukaavaa "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_02_Quadratic_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve, without using the quadratic formula, the quadratic equation "
                          ES -> "Resolver la ecuación cuadrática sin utilizar la fórmula cuadrática "
                          DE -> "Lösen Sie die quadratische Gleichung ohne Benutzung der p-q-Formel "
                          FR -> "Résoudre l'équation quadratique, sans utiliser la formule de résolution "
                          NL -> "Los de volgende kwadratische vergelijking op, zonder gebruik te maken van de abc-formule "
                          FI -> "Ratkaise toisen asteen yhtälö käyttämättä ratkaisukaavaa "
                          HU -> ""
  , problemStatement  = "Solve, without using the quadratic formula, the following quadratic equation: "
  , context           = showId $ exerciseId quadraticNoABCExercise
  , difficulty        = "easy"
  }
  
quadraticWithApproximationExerciseInfo :: MBExerciseInfo
quadraticWithApproximationExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving quadratic equations (with approximation)"
                          ES -> "Resolver la ecuación cuadrática, con aproximación permitida "
                          DE -> "Lösen Sie die quadratische Gleichung"
                          FR -> "Résoudre numériquement une équation quadratique "
                          NL -> "Kwadratische vergelijkingen oplossen (met benaderingen)"
                          FI -> "Toisen asteen yhtälöiden likimääräinen ratkaiseminen "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_02_Quadratic_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve, with approximation allowed, the quadratic equation "
                          ES -> "Resolver la ecuación cuadrática, con aproximación permitida "
                          DE -> "Lösen Sie die quadratische Gleichung"
                          FR -> "Résoudre l'équation quadratique, approximation permise "
                          NL -> "Los de volgende kwadratische vergelijking op, een benadering is toegestaan "
                          FI -> "Ratkaise likimääräisesti toisen asteen yhtälö "
                          HU -> ""
  , problemStatement  = "Solve, with approximation allowed, the following quadratic equation: "
  , context           = showId $ exerciseId quadraticWithApproximation
  , difficulty        = "easy"
  }

rationalEquationExerciseInfo :: MBExerciseInfo
rationalEquationExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving rational equations"
                          ES -> "Resolver la ecuación racional "
                          DE -> "Lösen Sie die Bruchgleichung "
                          FR -> "Équation rationnelle "
                          NL -> "Gebroken vergelijkingen oplossen"
                          FI -> "Rationaaliyhtälöiden ratkaiseminen "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_04_Rational_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Solve the rational equation "
                          ES -> "Resolver la ecuación racional "
                          DE -> "Lösen Sie die Bruchgleichung "
                          FR -> "Résoudre l'équation rationnelle "
                          NL -> "Los de volgende gebroken vergelijking op "
                          FI -> "Ratkaise rationaaliyhtälö "
                          HU -> ""
  , problemStatement  = "Solve the following rational equation: "
  , context           = showId $ exerciseId rationalEquationExercise
  , difficulty        = "medium"
  }

simplifyPowerExerciseInfo :: MBExerciseInfo
simplifyPowerExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Simplifying powers"
                          ES -> "Simplificar las potencias "
                          DE -> "Vereinfachen Sie die Potenz "
                          FR -> "Simplification de puissance "
                          NL -> "Machten vereenvoudigen"
                          FI -> "Potenssilausekkeiden sieventäminen "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_02_Algebraic_Manipulation"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Simplify the power "
                          ES -> "Simplificar las potencias "
                          DE -> "Vereinfachen Sie die Potenz "
                          FR -> "Simplifier la puissance "
                          NL -> "Vereenvoudig de macht "
                          FI -> "Sievennä potenssilauseke "
                          HU -> ""
  , problemStatement  = "Simplify the following power: "
  , context           = showId $ exerciseId simplifyPowerExercise
  , difficulty        = "medium"
  }

simplifyRationalExerciseInfo :: MBExerciseInfo
simplifyRationalExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Simplifying rationals"
                          ES -> "Simplificar el racional "
                          DE -> "Vereinfachen Sie den Bruch "
                          FR -> "Simplification de rationnel "
                          NL -> "Breuken vereenvoudigen"
                          FI -> "Murtolausekkeiden sieventäminen"
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_02_Algebraic_Manipulation"
  , langSupported     = [EN,ES,DE,FR,NL,FI]
  , cmp               = \l -> case l of 
                          EN -> "Simplify the rational "
                          ES -> "Simplificar el racional "
                          DE -> "Vereinfachen Sie den Bruch "
                          FR -> "Simplifiez le rationnel "
                          NL -> "Vereenvoudig de breuk "
                          FI -> "Sievennä murtolauseke "
                          HU -> ""
  , problemStatement  = "Simplify the following rational: "
  , context           = showId $ exerciseId simplifyRationalExercise
  , difficulty        = "medium"
  }

systemWithMatrixExerciseInfo :: MBExerciseInfo
systemWithMatrixExerciseInfo = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving systems of linear equations using matrices"
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Systemen van lineaire vergelijkingen oplossen mbv. matrices"
                          FI -> "" 
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_02_02_Matrix_Algebra"
  , langSupported     = [EN]
  , cmp               = \l -> case l of 
                          EN -> "Solve the following system of linear equations using a matrix "
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Los het volgende systeem van lineaire vergelijkingen op mbv. matrices "
                          FI -> "" 
                          HU -> ""
  , problemStatement  = "Solve the following system of linear equations using a matrix:  "
  , context           = showId $ exerciseId systemWithMatrixExercise
  , difficulty        = ""
  }
