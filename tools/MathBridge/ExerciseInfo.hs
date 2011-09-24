-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  johan.jeuring@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Module specifying the texts for the ideas exercises used in Math-Bridge
--
-----------------------------------------------------------------------------
module ExerciseInfo where

import Languages

import Common.Id

--------------------------------------------------------------------------------
{- General info about Ideas exercises for Math-Bridge
-}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
{- Info about particular exercises for Math-Bridge
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

-- For cmp I could reuse the linearExerciseInfo
balanceExerciseInfo :: Id -> MBExerciseInfo
balanceExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving linear equations using balancing"
                          NL -> "Lineaire vergelijkingen oplossen met de belanceermethode"
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_01_Linear_Equations"
  , langSupported     = [EN,NL]
  , cmp               = \l -> case l of
                          EN -> "Solve the linear equation "
                          NL -> "Los de volgende lineaire vergelijking op "
  , problemStatement  = "Solve the following linear equation: "
  , context           = showId $ identifier 
  , difficulty        = "medium"
  }

calcPowerExerciseInfo :: Id -> MBExerciseInfo
calcPowerExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Calculating powers"
                          ES -> "Calcular la potencia "
                          DE -> "Aufgaben zu Potenzen "
                          FR -> "Calculer avec des puissances "
                          NL -> "Machten berekenen"
                          FI -> "Potenssien laskeminen "
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_numbers_and_computation/_01_02_05_03_Powers"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Calculate the power "
                          ES -> "Calcular la potencia "
                          DE -> "Berechnen Sie die Potenz "
                          FR -> "Calculer la puissance "
                          NL -> "Bereken de macht "
                          FI -> "Laske arvo potenssilausekkeelle "
                          HU -> "Számítsa ki a kitevőt"
  , problemStatement  = "Calculate the following power: "
  , context           = showId $ identifier 
  , difficulty        = "medium"
  }

coverUpExerciseInfo :: Id -> MBExerciseInfo
coverUpExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving equations"
                          ES -> "Resolver la ecuación "
                          DE -> "Aufgaben zum Lösen von Gleichungen "
                          FR -> "Résoudre des équations "
                          NL -> "Vergelijkingen oplossen"
                          FI -> "Yhtälöiden ratkaiseminen "
                          HU -> "Bevezető feladat algebrai egyenletekhez"
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Solve the equation "
                          ES -> "Resolver la ecuación "
                          DE -> "Lösen Sie die Gleichung "
                          FR -> "Résoudre l'équation "
                          NL -> "Los de volgende vergelijking op "
                          FI -> "Ratkaise yhtälö "
                          HU -> "Oldja meg az egyenletet "
  , problemStatement  = "Solve the following equation: "
  , context           = showId $ identifier 
  , difficulty        = "medium"
  }

derivativeExerciseInfo :: Id -> MBExerciseInfo
derivativeExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Derivatives"
                          ES -> "Calcular la derivada de la función "
                          DE -> "Aufgaben zum Ableiten (Gemischt) "
                          FR -> "Dérivées "
                          NL -> "Afgeleiden"
                          FI -> "Derivaatat "
                          HU -> "Feladat deriválásához"
  , for               = "mbase://mb_concepts/mb_calculus/_06_01_04_Differentiation"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Calculate the following derivative of a function "
                          ES -> "Calcular la siguiente derivada de la función "
                          DE -> "Bestimmen Sie die folgende Ableitung einer Funktion "
                          FR -> "Calculer la dérivée de la fonction "
                          NL -> "Bereken de volgende afgeleide van een functie "
                          FI -> "Laske seuraava derivaatta "
                          HU -> "Deriválja a függvényt"
  , problemStatement  = "Calculate the derivative of the following function: "
  , context           = showId $ identifier 
  , difficulty        = "medium"
  }

derivativePolyExerciseInfo :: Id -> MBExerciseInfo
derivativePolyExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Differentiate polynomials"
                          ES -> "Calcular la derivada del polinomio "
                          DE -> "Aufgaben zum Ableiten (Polynome) "
                          FR -> "Dérivée d'un polynôme "
                          NL -> "Afgeleiden van polynomen"
                          FI -> "Derivaatan laskeminen polynomille "
                          HU -> "Feladat polinomok deriválásához"
  , for               = "mbase://mb_concepts/mb_calculus/_06_01_04_Differentiation"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Calculate the following derivative of a polynomial "
                          ES -> "Calcular la siguiente derivada del polinomio "
                          DE -> "Bestimmen Sie die folgende Ableitung eines Produkts von Funktionen "
                          FR -> "Calculer la dérivée du polynôme "
                          NL -> "Bereken de volgende afgeleide van een polynoom "
                          FI -> "Laske seuraava derivaatta "
                          HU -> "Deriválja a polinomot "
  , problemStatement  = "Calculate the derivative of the following polynomial: "
  , context           = showId $ identifier 
  , difficulty        = "medium"
  }

derivativeProductExerciseInfo :: Id -> MBExerciseInfo
derivativeProductExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Differentiate products"
                          ES -> "Calcular la derivada del producto "
                          DE -> "Aufgaben zum Ableiten (Produktregel) "
                          FR -> "Dérivée d'un produit "
                          NL -> "Afgeleiden van producten"
                          FI -> "Derivaatan laskeminen tulolle "
                          HU -> "Feladat szorzat deriválásához"
  , for               = "mbase://mb_concepts/mb_calculus/_06_01_04_Differentiation"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Calculate the following derivative of a product "
                          ES -> "Calcular la siguiente derivada del producto "
                          DE -> "Bestimmen Sie die folgende Ableitung eines Produkts von Funktionen "
                          FR -> "Calculer la dérivée du produit "
                          NL -> "Bereken de volgende afgeleide van een product "
                          FI -> "Laske seuraava derivaatta "
                          HU -> "Deriválja a szorzatot"
  , problemStatement  = "Calculate the derivative of the following product: "
  , context           = showId $ identifier 
  , difficulty        = "medium"
  }

derivativeQuotientExerciseInfo :: Id -> MBExerciseInfo
derivativeQuotientExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Differentiate quotients"
                          ES -> "Calcular la derivada del cociente "
                          DE -> "Aufgaben zum Ableiten (Quotientenregel) "
                          FR -> "Dérivée d'un quotient "
                          NL -> "Afgeleiden van quotiënten"
                          FI -> "Derivaatan laskeminen osamäärälle "
                          HU -> "Feladat tört deriválásához"
  , for               = "mbase://mb_concepts/mb_calculus/_06_01_04_Differentiation"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Calculate the following derivative of a quotient "
                          ES -> "Calcular la siguiente derivada del cociente "
                          DE -> "BBestimmen Sie die folgende Ableitung eines Quotienten von Funktionen "
                          FR -> "Calculer la dérivée du quotient "
                          NL -> "Bereken de volgende afgeleide van een quotiënt "
                          FI -> "Laske derivaatta osamäärälle "
                          HU -> "Deriválja a törtet"
  , problemStatement  = "Calculate the derivative of the following quotient: "
  , context           = showId $ identifier 
  , difficulty        = "medium"
  }

expandExerciseInfo :: Id -> MBExerciseInfo
expandExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Expanding polynomial expressions"
                          NL -> "Polynomen uitwerken"
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_02_Algebraic_Manipulation"
  , langSupported     = [EN,NL]
  , cmp               = \l -> case l of
                          EN -> "Expand the following expression "
                          NL -> "Werk de haken weg in de volgende expressie "
  , problemStatement  = "Expand the following expression: "
  , context           = showId $ identifier 
  , difficulty        = "medium"
  }

expEqExerciseInfo :: Id -> MBExerciseInfo
expEqExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving exponential equations"
                          ES -> "Resolver la ecuación exponencial "
                          DE -> "Aufgaben zum Lösen von Exponentialgleichungen "
                          FR -> "Équation exponentielle "
                          NL -> "Exponentiële vergelijking oplossen"
                          FI -> "Eksponenttiyhtälöiden ratkaiseminen "
                          HU -> "Feladat exponenciális egyenletekhez"
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_05_Exponential_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Solve the exponential equation "
                          ES -> "Resolver la ecuación exponencial "
                          DE -> "Lösen Sie die Exponentialgleichung "
                          FR -> "Résoudre l'équation exponentielle "
                          NL -> "Los de volgende exponentiële vergelijking op "
                          FI -> "Ratkaise eksponenttiyhtälö "
                          HU -> "Oldja meg az exponenciális egyenetet "
  , problemStatement  = "Solve the following exponential equation: "
  , context           = showId $ identifier 
  , difficulty        = "medium"
  }

findFactorsExerciseInfo :: Id -> MBExerciseInfo
findFactorsExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Finding factors"
                          ES -> "Encontrar los factores "
                          DE -> "Aufgaben zum Ausklammern "
                          FR -> "Trouver des facteurs "
                          NL -> "Ontbinden in factoren"
                          FI -> "Kertoimien ratkaiseminen "
                          HU -> "Feladat polinomok szorzattá alakításához"
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_02_Algebraic_Manipulation"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Find the factors "
                          ES -> "Encontrar los factores "
                          DE -> "Finden Sie die Faktoren "
                          FR -> "Trouver les facteurs "
                          NL -> "Ontbind in factoren "
                          FI -> "Ratkaise kertoimet "
                          HU -> "Keresse meg gyöktényezőket"
  , problemStatement  = "Find the factors: "
  , context           = showId $ identifier 
  , difficulty        = "easy"
  }

fractionExerciseInfo :: Id -> MBExerciseInfo
fractionExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Simplifying fractions"
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Breuken vereenvoudigen"
                          FI -> ""
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_numbers_and_computation/_01_02_02_Fractions"
  , langSupported     = [EN,NL]
  , cmp               = \l -> case l of
                          EN -> "Simplify the fraction "
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Vereenvoudig de breuk "
                          FI -> ""
                          HU -> ""
  , problemStatement  = "Simplify the following fraction: "
  , context           = showId $ identifier 
  , difficulty        = ""
  }

gaussianElimExerciseInfo :: Id -> MBExerciseInfo
gaussianElimExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Gaussian elimination"
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Matrix vegen"
                          FI -> ""
                          HU -> "Feladat mátrixegyenletek Gauss módszerrel történő megoldásához"
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_02_02_Matrix_Algebra"
  , langSupported     = [EN,NL]
  , cmp               = \l -> case l of
                          EN -> "Perform Gaussian elimination on "
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Veeg de matrix "
                          FI -> ""
                          HU -> ""
  , problemStatement  = "Perform Gaussian elimination: "
  , context           = showId $ identifier 
  , difficulty        = ""
  }

gramSchmidtExerciseInfo :: Id -> MBExerciseInfo
gramSchmidtExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Gram Schmidt"
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Gram Schmidt"
                          FI -> ""
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_02_04_Vector_Spaces"
  , langSupported     = [EN,NL]
  , cmp               = \l -> case l of
                          EN -> "Solve using Gram Schmidt "
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Los op mbv. Gram Schmidt"
                          FI -> ""
                          HU -> ""
  , problemStatement  = "Solve using Gram Schmidt: "
  , context           = showId $ identifier 
  , difficulty        = ""
  }

higherDegreeExerciseInfo :: Id -> MBExerciseInfo
higherDegreeExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving higher degree polynomial equations"
                          ES -> "Resolver la ecuación de orden superior "
                          DE -> "Aufgaben zum Lösen von Polynom-Gleichungen "
                          FR -> "Équation polynomiale "
                          NL -> "Hogeregraads polynoomvergelijkingen oplossen"
                          FI -> "Korkeamman asteen polynomiyhtälön ratkaiseminen "
                          HU -> "Feladat ásodfokú egyenletekhez"
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_03_Polynomial_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Solve the higher degree polynomial equation "
                          ES -> "Resolver la ecuación de orden superior "
                          DE -> "Lösen Sie die Gleichung "
                          FR -> "Résoudre l'équation polynomiale de degré élevé "
                          NL -> "Los de volgende hogeregraads polynoomvergelijking op "
                          FI -> "Ratkaise korkeamman asteen polynomiyhätlö "
                          HU -> "Oldja meg a magasabb fokú egyenletet "
  , problemStatement  = "Solve the following higher degree polynomial equation: "
  , context           = showId $ identifier 
  , difficulty        = "easy"
  }

ineqHigherDegreeExerciseInfo :: Id -> MBExerciseInfo
ineqHigherDegreeExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving inequations of higher degree"
                          ES -> "Resolver la inecuación "
                          DE -> "Aufgaben zum Lösen von Polynom-Ungleichungen "
                          FR -> "Inéquation de degré supérieur "
                          NL -> "Hogeregraads ongelijkheden oplossen "
                          FI -> "Korkeamman asteen epäyhtälöiden ratkaiseminen "
                          HU -> "Feladat másodfokú egyenlőtlenségekhez "
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_05_Inequalities"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Solve the inequation "
                          ES -> "Resolver la inecuación "
                          DE -> "Lösen Sie die Ungleichung "
                          FR -> "Résoudre l'inéquation "
                          NL -> "Los de volgende ongelijkheid op "
                          FI -> "Ratkaise epäyhtälö "
                          HU -> ""
  , problemStatement  = "Solve the following inequation: "
  , context           = showId $ identifier 
  , difficulty        = "medium"
  }

ineqLinearExerciseInfo :: Id -> MBExerciseInfo
ineqLinearExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving linear inequations"
                          ES -> "Resolver la inecuación linea "
                          DE -> "Aufgaben zum Lösen von linearen Ungleichungen "
                          FR -> "Inéquations linéaires "
                          NL -> "Lineaire ongelijkheden oplossen"
                          FI -> "Lineaaristen epäyhtälöiden ratkaiseminen "
                          HU -> "Feladat magasabb fokú egyenlőtlenségekhez"
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_05_Inequalities"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Solve the linear inequation "
                          ES -> "Resolver la inecuación linea "
                          DE -> "Lösen Sie die lineare Ungleichung "
                          FR -> "Résoudre l'inéquation linéaire "
                          NL -> "Los de volgende lineaire ongelijkheid op "
                          FI -> "Ratkaise lineaarinen epäyhtälö "
                          HU -> "Oldja meg az elsőfokú egyenlőtlenséget "
  , problemStatement  = "Solve the following linear inequation: "
  , context           = showId $ identifier 
  , difficulty        = "medium"
  }

ineqQuadraticExerciseInfo :: Id -> MBExerciseInfo
ineqQuadraticExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving quadratic inequations"
                          ES -> "Resolver la inecuación "
                          DE -> "Aufgaben zum Lösen von quadratischen Ungleichungen "
                          FR -> "Inéquation du second degré "
                          NL -> "Kwadratische ongelijkheden oplossen"
                          FI -> "Kvadraattisien epäyhtälöiden ratkaiseminen "
                          HU -> "Feladat másodfokú egyenlőtlenségekhez"
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_05_Inequalities"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Solve the inequation "
                          ES -> "Resolver la inecuación "
                          DE -> "Lösen Sie die Ungleichung "
                          FR -> "Résoudre l'inéquation "
                          NL -> "Los de volgende ongelijkheid op "
                          FI -> "Ratkaise epäyhtälö "
                          HU -> "Oldja meg az egyenlőtlenséget "
  , problemStatement  = "Solve the following inequation: "
  , context           = showId $ identifier 
  , difficulty        = "medium"
  }

linearExerciseInfo :: Id -> MBExerciseInfo
linearExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving linear equations"
                          ES -> "Resolver la ecuación lineal "
                          DE -> "Aufgaben zum Lösen von linearen Gleichungen "
                          FR -> "Équation linéaire "
                          NL -> "Lineaire vergelijkingen oplossen"
                          FI -> "Lineaaristen yhtälöiden ratkaiseminen "
                          HU -> "Könnyű feladat elsőfokú egyenletekhez "
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_01_Linear_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Solve the linear equation "
                          ES -> "Resolver la ecuación lineal "
                          DE -> "Lösen Sie die lineare Gleichung "
                          FR -> "Résoudre l'équation linéaire "
                          NL -> "Los de volgende lineaire vergelijking op "
                          FI -> "Ratkaise lineaarinen yhtälö "
                          HU -> "Oldja meg az elsőfokú egyenletet "
  , problemStatement  = "Solve the following equation: "
  , context           = showId $ identifier 
  , difficulty        = "easy"
  }

linearMixedExerciseInfo :: Id -> MBExerciseInfo
linearMixedExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving linear mixed equations"
                          ES -> "Resolver la ecuación lineal compuesta "
                          DE -> "Aufgaben zum Lösen von vermischten linearen Gleichungen "
                          FR -> "Équation linéaire mixte "
                          NL -> "Gemengde lineaire vergelijkingen oplossen"
                          FI -> "Lineaaristen sekayhtälöiden ratkaisu "
                          HU -> "Feladatk magasabb fokú egyenletekhez "
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_01_Linear_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Solve the linear mixed equation "
                          ES -> "Resolver la ecuación lineal compuesta "
                          DE -> "Lösen Sie die lineare Gleichung "
                          FR -> "Résoudre l'équation linéaire mixte "
                          NL -> "Los de volgende gemengde lineaire vergelijking op "
                          FI -> "Ratkaise lineaarinen yhtälö "
                          HU -> "Oldja meg az elsőfokú egyenletet "
  , problemStatement  = "Solve the linear mixed equation: "
  , context           = showId $ identifier 
  , difficulty        = "easy"
  }

linearSystemExerciseInfo :: Id -> MBExerciseInfo
linearSystemExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving systems of linear equations"
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Systemen van lineaire vergelijkingen oplossen"
                          FI -> ""
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_07_Systems_of_Equations"
  , langSupported     = [EN,NL]
  , cmp               = \l -> case l of
                          EN -> "Solve the system of linear equations "
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Los het volgende systeem van lineaire vergelijkingen op"
                          FI -> ""
                          HU -> ""
  , problemStatement  = "Solve the following system of linear equations: "
  , context           = showId $ identifier 
  , difficulty        = ""
  }

logEqExerciseInfo :: Id -> MBExerciseInfo
logEqExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving logarithmic equations"
                          ES -> "Resolver la ecuación logarítmica "
                          DE -> "Aufgaben zum Lösen von logarithmischen Gleichungen "
                          FR -> "Équation logarithmique "
                          NL -> "Logarithmische vergelijkingen oplossen"
                          FI -> "Logaritmiyhtälöiden ratkaisu "
                          HU -> "Feladat logaritmikus egyenletekhez"
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_06_Logarithmic_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Solve the logarithmic equation "
                          ES -> "Resolver la ecuación logarítmica "
                          DE -> "Lösen Sie die Gleichung mit Logarithmen "
                          FR -> "Résoudre l'équation logarithmique "
                          NL -> "Los de volgende logaritmische vergelijking op "
                          FI -> "Ratkaise logaritmiyhtälö "
                          HU -> "Oldja meg a logaritmusos egyenletet"
  , problemStatement  = "Solve the following logarithmic equation: "
  , context           = showId $ identifier 
  , difficulty        = "medium"
  }

nonNegBrokenExpExerciseInfo :: Id -> MBExerciseInfo
nonNegBrokenExpExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Writing with non-negative exponents"
                          ES -> "Expresar con exponente no negativo "
                          DE -> "Aufgaben zum Umformen von Potenzen "
                          FR -> "Écrire avec une puissance positive "
                          NL -> "Met niet-negatieve exponenten schrijven"
                          FI -> "Ilmaiseminen käyttäen ei-negatiivista eksponenttia "
                          HU -> "Feladat nemnegatív kitevőt tartalmazó kifejezésekhez"
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_02_Algebraic_Manipulation"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Write with a non-negative exponent "
                          ES -> "Expresar con exponente no negativo "
                          DE -> "Schreiben Sie mit nicht-negativen Exponenten "
                          FR -> "Écrire avec une puissance non-négative "
                          NL -> "Schrijf met niet-negatieve exponenten "
                          FI -> "Ilmaise käyttäen ei-negatiivista eksponenttia "
                          HU -> "Írja fel nemnegatív hatvány segítségével"
  , problemStatement  = "Write the following with a non-negative exponent: "
  , context           = showId $ identifier 
  , difficulty        = "medium"
  }

powerEqExerciseInfo :: Id -> MBExerciseInfo
powerEqExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving power equations"
                          ES -> "Resolver la ecuación con potencias "
                          DE -> "Aufgaben zum Lösen von Potenzgleichungen "
                          FR -> "Équation avec puissances "
                          NL -> "Machtsvergelijkingen oplossen"
                          FI -> "Potenssiyhtälöiden ratkaiseminen "
                          HU -> "Feladat exponenciális egyenletekhez (x > 0)"
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_03_Polynomial_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Solve the power equation "
                          ES -> "Resolver la ecuación con potencias "
                          DE -> "Lösen Sie die Gleichung mit x im Exponenten "
                          FR -> "Résoudre l'équation avec puissances "
                          NL -> "Los de volgende machtsvergelijking op "
                          FI -> "Ratkaise potenssiyhtälö "
                          HU -> "Oldja meg az egyenletet "
  , problemStatement  = "Solve the following power equation: "
  , context           = showId $ identifier 
  , difficulty        = "medium"
  }

powerOfExerciseInfo :: Id -> MBExerciseInfo
powerOfExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Writing as a power"
                          ES -> "Expresar como potencia "
                          DE -> "Aufgaben zum Zusammenfassen von Potenzen "
                          FR -> "Écrire sous la forme d'une puissance "
                          NL -> "Als een macht schrijven"
                          FI -> "Ilmaiseminen potenssimuodossa "
                          HU -> "Feladat kifejezés felírására ... hatványaként"
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_02_Algebraic_Manipulation"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Write as a power "
                          ES -> "Expresar como potencia "
                          DE -> "Schreiben Sie als Potenz "
                          FR -> "Écrire sous la forme d'une puissance "
                          NL -> "Schrijf als een macht "
                          FI -> "Ilmaise potenssimuodossa "
                          HU -> "Írja fel hatvány alakban"
  , problemStatement  = "Write the following as a power: "
  , context           = showId $ identifier 
  , difficulty        = "medium"
  }

quadraticExerciseInfo :: Id -> MBExerciseInfo
quadraticExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving quadratic equations"
                          ES -> "Resolver la ecuación cuadrática "
                          DE -> "Aufgaben zum Lösen von quadratischen Gleichungen "
                          FR -> "Équation du second degré "
                          NL -> "Kwadratische vergelijkingen oplossen"
                          FI -> "Toisen asteen yhtälöiden ratkaiseminen "
                          HU -> "Feladat tört egyenletekhez "
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_02_Quadratic_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Solve the quadratic equation "
                          ES -> "Resolver la ecuación cuadrática "
                          DE -> "Lösen Sie die quadratische Gleichung "
                          FR -> "Résoudre l'équation quadratique "
                          NL -> "Los de volgende kwadratische vergelijking op "
                          FI -> "Ratkaise toisen asteen yhtälö "
                          HU -> "Oldja meg a másodfokú egyenletet "
  , problemStatement  = "Solve the following quadratic equation: "
  , context           = showId $ identifier 
  , difficulty        = "easy"
  }

quadraticNoABCExerciseInfo :: Id -> MBExerciseInfo
quadraticNoABCExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving quadratic equations (no quadratic formula)"
                          ES -> "Resolver la ecuación cuadrática sin utilizar la fórmula cuadrática "
                          DE -> "Aufgaben zum Lösen von quadratischen Gleichungen ohne die p-q-Formel "
                          FR -> "Résoudre une équation quadratique, sans utiliser la formule de résolution "
                          NL -> "Kwadratische vergelijkingen oplossen (geen abc)"
                          FI -> "Toisen asteen yhtälöiden ratkaiseminen käyttämättä ratkaisukaavaa "
                          HU -> "Feladat tört egyenletekhez "
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_02_Quadratic_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Solve, without using the quadratic formula, the quadratic equation "
                          ES -> "Resolver la ecuación cuadrática sin utilizar la fórmula cuadrática "
                          DE -> "Lösen Sie die quadratische Gleichung ohne Benutzung der p-q-Formel "
                          FR -> "Résoudre l'équation quadratique, sans utiliser la formule de résolution "
                          NL -> "Los de volgende kwadratische vergelijking op, zonder gebruik te maken van de abc-formule "
                          FI -> "Ratkaise toisen asteen yhtälö käyttämättä ratkaisukaavaa "
                          HU -> "Oldja meg a másodfokú egyenlet megoldóképlete nélkül az egyenletet "
  , problemStatement  = "Solve, without using the quadratic formula, the following quadratic equation: "
  , context           = showId $ identifier 
  , difficulty        = "easy"
  }

quadraticWithApproximationExerciseInfo :: Id -> MBExerciseInfo
quadraticWithApproximationExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving quadratic equations (with approximation)"
                          ES -> "Resolver la ecuación cuadrática, con aproximación permitida "
                          DE -> "Aufgaben zum Lösen von quadratischen Gleichungen (näherungsweise Lösung erlaubt) "
                          FR -> "Résoudre numériquement une équation quadratique "
                          NL -> "Kwadratische vergelijkingen oplossen (met benaderingen)"
                          FI -> "Toisen asteen yhtälöiden likimääräinen ratkaiseminen "
                          HU -> "Feladat tört egyenletekhez "
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_02_Quadratic_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Solve, with approximation allowed, the quadratic equation "
                          ES -> "Resolver la ecuación cuadrática, con aproximación permitida "
                          DE -> "Lösen Sie die quadratische Gleichung"
                          FR -> "Résoudre l'équation quadratique, approximation permise "
                          NL -> "Los de volgende kwadratische vergelijking op, een benadering is toegestaan "
                          FI -> "Ratkaise likimääräisesti toisen asteen yhtälö "
                          HU -> "Oldja meg a másodfokú egyenletet, közelítő módszer megengedett "
  , problemStatement  = "Solve, with approximation allowed, the following quadratic equation: "
  , context           = showId $ identifier 
  , difficulty        = "easy"
  }

rationalEquationExerciseInfo :: Id -> MBExerciseInfo
rationalEquationExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving rational equations"
                          ES -> "Resolver la ecuación racional "
                          DE -> "Aufgaben zum Lösen von Bruchgleichungen "
                          FR -> "Équation rationnelle "
                          NL -> "Gebroken vergelijkingen oplossen"
                          FI -> "Rationaaliyhtälöiden ratkaiseminen "
                          HU -> "Feladat elsőfokú egyenlőtlenségekhez "
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_04_04_Rational_Equations"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Solve the rational equation "
                          ES -> "Resolver la ecuación racional "
                          DE -> "Lösen Sie die Bruchgleichung "
                          FR -> "Résoudre l'équation rationnelle "
                          NL -> "Los de volgende gebroken vergelijking op "
                          FI -> "Ratkaise rationaaliyhtälö "
                          HU -> "Oldja meg a törtegyütthatós egyenletet "
  , problemStatement  = "Solve the following rational equation: "
  , context           = showId $ identifier 
  , difficulty        = "medium"
  }

simplifyPowerExerciseInfo :: Id -> MBExerciseInfo
simplifyPowerExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Simplifying powers"
                          ES -> "Simplificar las potencias "
                          DE -> "Aufgaben zum Vereinfachen von Potenzen "
                          FR -> "Simplification de puissance "
                          NL -> "Machten vereenvoudigen"
                          FI -> "Potenssilausekkeiden sieventäminen "
                          HU -> "Feladat hatványokat tartalmazó kifejezések egyszerűsítéséhez"
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_02_Algebraic_Manipulation"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Simplify the power "
                          ES -> "Simplificar las potencias "
                          DE -> "Vereinfachen Sie die Potenz "
                          FR -> "Simplifier la puissance "
                          NL -> "Vereenvoudig de macht "
                          FI -> "Sievennä potenssilauseke "
                          HU -> "Egyszerűsítse a kitevőt "
  , problemStatement  = "Simplify the following power: "
  , context           = showId $ identifier 
  , difficulty        = "medium"
  }

simplifyRationalExerciseInfo :: Id -> MBExerciseInfo
simplifyRationalExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Simplifying rationals"
                          ES -> "Simplificar el racional "
                          DE -> "Aufgaben zum Vereinfachen von Brüchen "
                          FR -> "Simplification de rationnel "
                          NL -> "Breuken vereenvoudigen"
                          FI -> "Murtolausekkeiden sieventäminen"
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_01_02_Algebraic_Manipulation"
  , langSupported     = [EN,ES,DE,FR,NL,FI,HU]
  , cmp               = \l -> case l of
                          EN -> "Simplify the rational "
                          ES -> "Simplificar el racional "
                          DE -> "Vereinfachen Sie den Bruch "
                          FR -> "Simplifiez le rationnel "
                          NL -> "Vereenvoudig de breuk "
                          FI -> "Sievennä murtolauseke "
                          HU -> "Egyszerűsítse a törtet"
  , problemStatement  = "Simplify the following rational: "
  , context           = showId $ identifier 
  , difficulty        = "medium"
  }

systemWithMatrixExerciseInfo :: Id -> MBExerciseInfo
systemWithMatrixExerciseInfo identifier = MBExerciseInfo
  { title             = \l -> case l of
                          EN -> "Solving systems of linear equations using matrices"
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Systemen van lineaire vergelijkingen oplossen mbv. matrices"
                          FI -> ""
                          HU -> ""
  , for               = "mbase://mb_concepts/mb_algebra_and_number_theory/_03_02_02_Matrix_Algebra"
  , langSupported     = [EN,NL]
  , cmp               = \l -> case l of
                          EN -> "Solve the following system of linear equations using a matrix "
                          ES -> ""
                          DE -> ""
                          FR -> ""
                          NL -> "Los het volgende systeem van lineaire vergelijkingen op mbv. matrices "
                          FI -> ""
                          HU -> ""
  , problemStatement  = "Solve the following system of linear equations using a matrix:  "
  , context           = showId $ identifier 
  , difficulty        = ""
  }