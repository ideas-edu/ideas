module Service.Omdoc where

-- revision toevoegen aan gegenereerde files
-- spul integreren met ideas

import Data.Map(Map,empty,insert,(!))

import Common.Exercise
-- import Common.Id
import Common.Rewriting.Term
import Service.ExercisePackage
import Text.OpenMath.Object
import Text.XML.Interface

import Domain.LinearAlgebra
import Domain.Math.Polynomial.Exercises
import Domain.Math.Derivative.Exercises
import Domain.Math.Numeric.Exercises
import Domain.Math.Polynomial.RationalExercises
import Domain.Math.Polynomial.IneqExercises
import Domain.Math.Equation.CoverUpExercise
import Domain.Math.Power.Exercises
import Domain.Math.Power.Equation.Exercises

import Text.XML

--------------------------------------------------------------------------------
{- Generate all files
-}
--------------------------------------------------------------------------------

generateallfiles :: IO ()
generateallfiles = 
  do omdocexercisefile linearExercise
     omdocexercisefile linearMixedExercise
     omdocexercisefile quadraticExercise
     omdocexercisefile higherDegreeExercise
     omdocexercisefile rationalEquationExercise
     omdocexercisefile ineqLinearExercise
     omdocexercisefile coverUpExercise
     omdocexercisefile fractionExercise
     omdocexercisefile findFactorsExercise
     omdocexercisefile ineqQuadraticExercise
     omdocexercisefile ineqHigherDegreeExercise
     omdocexercisefile simplifyRationalExercise
     omdocexercisefile quadraticNoABCExercise
     omdocexercisefile quadraticWithApproximation
     omdocexercisefile derivativeExercise
     omdocexercisefile derivativePolyExercise
     omdocexercisefile derivativeProductExercise
     omdocexercisefile derivativeQuotientExercise
     omdocexercisefile simplifyPowerExercise
     omdocexercisefile powerOfExercise
     omdocexercisefile nonNegBrokenExpExercise
     omdocexercisefile calcPowerExercise
     omdocexercisefile powerEqExercise
     omdocexercisefile expEqExercise
     omdocexercisefile logEqExercise
     omdocexercisefile gramSchmidtExercise
     omdocexercisefile linearSystemExercise
     omdocexercisefile gaussianElimExercise
     omdocexercisefile systemWithMatrixExercise
     recbookfile
       [sourcefile linearExercise
       ,sourcefile linearMixedExercise
       ,sourcefile quadraticExercise
       ,sourcefile higherDegreeExercise
       ,sourcefile rationalEquationExercise
       ,sourcefile ineqLinearExercise
       ,sourcefile coverUpExercise
       ,sourcefile fractionExercise
       ,sourcefile findFactorsExercise
       ,sourcefile ineqQuadraticExercise
       ,sourcefile ineqHigherDegreeExercise
       ,sourcefile simplifyRationalExercise
       ,sourcefile quadraticNoABCExercise
       ,sourcefile quadraticWithApproximation
       ,sourcefile derivativeExercise
       ,sourcefile derivativePolyExercise
       ,sourcefile derivativeProductExercise
       ,sourcefile derivativeQuotientExercise
       ,sourcefile simplifyPowerExercise
       ,sourcefile powerOfExercise
       ,sourcefile nonNegBrokenExpExercise
       ,sourcefile calcPowerExercise
       ,sourcefile powerEqExercise
       ,sourcefile expEqExercise
       ,sourcefile logEqExercise
       ,sourcefile gramSchmidtExercise
       ,sourcefile linearSystemExercise
       ,sourcefile gaussianElimExercise
       ,sourcefile systemWithMatrixExercise
       ]
       [omdocexerciserefs linearExercise
       ,omdocexerciserefs linearMixedExercise
       ,omdocexerciserefs quadraticExercise
       ,omdocexerciserefs higherDegreeExercise
       ,omdocexerciserefs rationalEquationExercise
       ,omdocexerciserefs ineqLinearExercise
       ,omdocexerciserefs coverUpExercise
       ,omdocexerciserefs fractionExercise
       ,omdocexerciserefs findFactorsExercise
       ,omdocexerciserefs ineqQuadraticExercise
       ,omdocexerciserefs ineqHigherDegreeExercise
       ,omdocexerciserefs simplifyRationalExercise
       ,omdocexerciserefs quadraticNoABCExercise
       ,omdocexerciserefs quadraticWithApproximation
       ,omdocexerciserefs derivativeExercise
       ,omdocexerciserefs derivativePolyExercise
       ,omdocexerciserefs derivativeProductExercise
       ,omdocexerciserefs derivativeQuotientExercise
       ,omdocexerciserefs simplifyPowerExercise
       ,omdocexerciserefs powerOfExercise
       ,omdocexerciserefs nonNegBrokenExpExercise
       ,omdocexerciserefs calcPowerExercise
       ,omdocexerciserefs powerEqExercise
       ,omdocexerciserefs expEqExercise
       ,omdocexerciserefs logEqExercise
       ,omdocexerciserefs gramSchmidtExercise
       ,omdocexerciserefs linearSystemExercise
       ,omdocexerciserefs gaussianElimExercise
       ,omdocexerciserefs systemWithMatrixExercise
       ]

--------------------------------------------------------------------------------
{- Generating the recbook for the exercise collections
-}
--------------------------------------------------------------------------------

recbookfile :: [Element] -> [Element] -> IO ()
recbookfile sourcefiles exercisesrefs =
  let -- sourcefiles    =  map sourcefile        exercises
      -- exercisesrefs  =  map omdocexerciserefs exercises
      filestring = 
             xmldecl 
          ++ activemathdtd 
          ++ showXML
               (omdocelt 
                  ""
                  "http://www.activemath.org/namespaces/am_internal"
                  [omgroupelt "" "http://www.mathweb.org/omdoc"
                    [omgroupelt "IdeasExercises" ""
                      [metadataelt "IdeasExercises-metadata"
                        [titleelt "Ideas Exercises collection"]
                      ,omgroupelt "recbook_for_IdeasExercises" ""
                        (metadataelt ""
                          [titleelt "Complete Ideas Exercises Recbook"
                          ,dateelt "created" "2011-01-22"
                          ,creatorelt "aut" "Johan Jeuring"
                          ,sourceelt ""
                          ,formatelt "application/omdoc+xml"
                          ,extradataelt
                            sourcefiles
	                      ]
                        :exercisesrefs
                        )
                      ]
                    ]
                  ]
               )
  in writeFile (omdocpath ++ "RecBook_Exercises.omdoc") filestring
  
sourcefile :: Exercise a -> Element
sourcefile ex = 
  sourcefileelt 
    "http://www.activemath.org/namespaces/am_internal" 
    ("omdoc/" ++ show (exerciseId ex))
    "1293473065000" -- last modified; don't know why or if this has to be provided

omdocrefpath  :: String
omdocrefpath  =  ""

omdocexerciserefs :: Exercise a -> Element
omdocexerciserefs ex = 
  let info             = mBExerciseInfo ! (exerciseId $ ex)
      len              = length (examples ex)
      refs             = map (\i -> omdocrefpath  -- relative dir (the same right now)
                                ++  context info  -- filename
                                ++  "/" 
                                ++  context info  -- exercise id
                                ++  show i)       -- and nr
                             [0..len-1]
      exercises        = map (\ ref -> refelt ref "exercise") refs
  in omgroupelt "" "" (metadataelt "" [titleelt (title info)]:exercises)

--------------------------------------------------------------------------------
{- Generating an omdoc file for an exercise
-}
--------------------------------------------------------------------------------

omdocpath :: String
omdocpath = "/Users/johanj/Documents/Research/ExerciseAssistants/Feedback/math-bridge/activemath/all/activemath-ideas/content/IdeasExercises/omdoc/"

omdocexercisefile :: (IsTerm a) => Exercise a -> IO ()
omdocexercisefile ex = 
  let info = mBExerciseInfo ! (exerciseId $ ex)
      filestring = 
             xmldecl 
          ++ activemathdtd 
          ++ showXML
               (omdocelt 
                  (context info ++ ".omdoc")
                  []
                  [metadataelt 
                    ""
                    [dateelt "created" "2011-01-22"
                    ,titleelt (title info)
                    ,creatorelt "aut" "Johan Jeuring"
                    ]
                  ,theoryelt (context info) 
                             (omdocexercises ex)
                  ]
               )
  in writeFile (omdocpath ++ context info ++ ".omdoc") filestring

omdocexercises :: (IsTerm a) => Exercise a -> [Element]
omdocexercises ex = 
  let info = mBExerciseInfo ! (exerciseId $ ex)
      exerciseexamples = zip [(0::Int)..] 
                             (map (omobj2xml
                                  .toOpenMath (termPackage ex)
                                  )
                                  (examples ex)
                                   )
      exercises        = map (\(i,exex) -> omdocexercise  
                                             (context info ++ show i)
                                             "easy" -- should be in the example somewhere
                                             "en"
                                             (cmpText info ++ show exex)
                                             "IDEASGenerator"
                                             "strategy"
                                             (problemStatement info)
                                             (context info)
                                             exex
                             )
                             exerciseexamples              
  in exercises

omdocexercise :: String
              -> String
              -> String
              -> String
              -> String
              -> String
              -> String
              -> String
              -> Element
              -> Element
omdocexercise 
    exerciseid 
    difficulty 
    lang 
    cmptext 
    interaction_generatorname 
    interaction_generatortype 
    problemstatement
    ctxt
    task
  = exerciseelt 
      exerciseid          
      [metadataelt
        ""
        [formatelt "AMEL1.0"
        ,extradataelt 
          [difficultyelt difficulty
          ]
        ]
      , cmpelt lang cmptext
      ,interaction_generatorelt 
        interaction_generatorname 
        interaction_generatortype
        [parameterelt "problemstatement" [Left problemstatement]
        ,parameterelt "context"          [Left ctxt]
        ,parameterelt "difficulty"       [Left difficulty]
        ,parameterelt "task"             [Right task]
        ]
      ]

--------------------------------------------------------------------------------
{- Info about exercises for ActiveMath
-}
--------------------------------------------------------------------------------

data MBExerciseInfo = MBExerciseInfo
  { title             :: String
  , cmpText           :: String
  , problemStatement  :: String
  , context           :: String
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
  $ empty

calcPowerExerciseInfo :: MBExerciseInfo
calcPowerExerciseInfo = MBExerciseInfo
  { title             = "Calculating powers"
  , cmpText           = "Calculate the power "
  , problemStatement  = "Calculate the following power: "
  , context           = showId $ exerciseId calcPowerExercise
  }

coverUpExerciseInfo :: MBExerciseInfo
coverUpExerciseInfo = MBExerciseInfo
  { title             = "Covering up in equations"
  , cmpText           = "Solve the equation "
  , problemStatement  = "Solve the following equation: "
  , context           = showId $ exerciseId coverUpExercise
  }

derivativeExerciseInfo :: MBExerciseInfo
derivativeExerciseInfo = MBExerciseInfo
  { title             = "Derivatives"
  , cmpText           = "Calculate the derivative of the function "
  , problemStatement  = "Calculate the derivative of the following function: "
  , context           = showId $ exerciseId derivativeExercise
  }

derivativePolyExerciseInfo :: MBExerciseInfo
derivativePolyExerciseInfo = MBExerciseInfo
  { title             = "Differentiate polynomials"
  , cmpText           = "Calculate the derivative of the polynomial "
  , problemStatement  = "Calculate the derivative of the following polynomial: "
  , context           = showId $ exerciseId derivativePolyExercise
  }

derivativeProductExerciseInfo :: MBExerciseInfo
derivativeProductExerciseInfo = MBExerciseInfo
  { title             = "Differentiate product"
  , cmpText           = "Calculate the derivative of the product "
  , problemStatement  = "Calculate the derivative of the following product: "
  , context           = showId $ exerciseId derivativeProductExercise
  }

derivativeQuotientExerciseInfo :: MBExerciseInfo
derivativeQuotientExerciseInfo = MBExerciseInfo
  { title             = "Differentiate quotients"
  , cmpText           = "Calculate the derivative of the quotient "
  , problemStatement  = "Calculate the derivative of the following quotient: "
  , context           = showId $ exerciseId derivativeQuotientExercise
  }

expEqExerciseInfo :: MBExerciseInfo
expEqExerciseInfo = MBExerciseInfo
  { title             = "Solving exponential equations"
  , cmpText           = "Solve the exponential equation "
  , problemStatement  = "Solve the following exponential equation: "
  , context           = showId $ exerciseId expEqExercise
  }

findFactorsExerciseInfo :: MBExerciseInfo
findFactorsExerciseInfo = MBExerciseInfo
  { title             = "Finding factors"
  , cmpText           = "Find the factors "
  , problemStatement  = "Find the factors: "
  , context           = showId $ exerciseId findFactorsExercise
  }

fractionExerciseInfo :: MBExerciseInfo
fractionExerciseInfo = MBExerciseInfo
  { title             = "Simplifying fractions"
  , cmpText           = "Simplify the fraction "
  , problemStatement  = "Simplify the following fraction: "
  , context           = showId $ exerciseId fractionExercise
  }

gaussianElimExerciseInfo :: MBExerciseInfo
gaussianElimExerciseInfo = MBExerciseInfo
  { title             = "Gaussian elimination"
  , cmpText           = "Perform Gaussian elimination "
  , problemStatement  = "Perform Gaussian elimination: "
  , context           = showId $ exerciseId gaussianElimExercise
  }

gramSchmidtExerciseInfo :: MBExerciseInfo
gramSchmidtExerciseInfo = MBExerciseInfo
  { title             = "Gram Schmidt"
  , cmpText           = "Solve using Gram Schmidt "
  , problemStatement  = "Solve using Gram Schmidt: "
  , context           = showId $ exerciseId gramSchmidtExercise
  }

higherDegreeExerciseInfo :: MBExerciseInfo
higherDegreeExerciseInfo = MBExerciseInfo
  { title             = "Solving higher degree polynomial equations"
  , cmpText           = "Solve the higher degree polynomial equation "
  , problemStatement  = "Solve the following higher degree polynomial equation: "
  , context           = showId $ exerciseId higherDegreeExercise
  }

ineqHigherDegreeExerciseInfo :: MBExerciseInfo
ineqHigherDegreeExerciseInfo = MBExerciseInfo
  { title             = "Solving inequations of higher degree"
  , cmpText           = "Solve the inequation "
  , problemStatement  = "Solve the following inequation: "
  , context           = showId $ exerciseId ineqHigherDegreeExercise
  }

ineqLinearExerciseInfo :: MBExerciseInfo
ineqLinearExerciseInfo = MBExerciseInfo
  { title             = "Solving linear inequations"
  , cmpText           = "Solve the linear inequation "
  , problemStatement  = "Solve the following linear inequation: "
  , context           = showId $ exerciseId ineqLinearExercise
  }

ineqQuadraticExerciseInfo :: MBExerciseInfo
ineqQuadraticExerciseInfo = MBExerciseInfo
  { title             = "Solving quadratic inequations"
  , cmpText           = "Solve the inequation "
  , problemStatement  = "Solve the following inequation: "
  , context           = showId $ exerciseId ineqQuadraticExercise
  }

linearExerciseInfo :: MBExerciseInfo
linearExerciseInfo = MBExerciseInfo
  { title             = "Solving linear equations"
  , cmpText           = "Solve the linear equation "
  , problemStatement  = "Solve the following equation: "
  , context           = showId $ exerciseId linearExercise
  }

linearMixedExerciseInfo :: MBExerciseInfo
linearMixedExerciseInfo = MBExerciseInfo
  { title             = "Solving linear mixed equations"
  , cmpText           = "Solve the linear mixed equation "
  , problemStatement  = "Solve the linear mixed equation: "
  , context           = showId $ exerciseId linearMixedExercise
  }

linearSystemExerciseInfo :: MBExerciseInfo
linearSystemExerciseInfo = MBExerciseInfo
  { title             = "Solving systems of linear equations"
  , cmpText           = "Solve the system of linear equations "
  , problemStatement  = "Solve the following system of linear equations: "
  , context           = showId $ exerciseId linearSystemExercise
  }

logEqExerciseInfo :: MBExerciseInfo
logEqExerciseInfo = MBExerciseInfo
  { title             = "Solving logarithmic equations"
  , cmpText           = "Solve the logarithmic equation "
  , problemStatement  = "Solve the following logarithmic equation: "
  , context           = showId $ exerciseId logEqExercise
  }

nonNegBrokenExpExerciseInfo :: MBExerciseInfo
nonNegBrokenExpExerciseInfo = MBExerciseInfo
  { title             = "Writing with non-negative exponents"
  , cmpText           = "Write with a non-negative exponent "
  , problemStatement  = "Write the following with a non-negative exponent: "
  , context           = showId $ exerciseId nonNegBrokenExpExercise
  }

powerEqExerciseInfo :: MBExerciseInfo
powerEqExerciseInfo = MBExerciseInfo
  { title             = "Solving power equations"
  , cmpText           = "Solve the power equation "
  , problemStatement  = "Solve the following power equation: "
  , context           = showId $ exerciseId powerEqExercise
  }

powerOfExerciseInfo :: MBExerciseInfo
powerOfExerciseInfo = MBExerciseInfo
  { title             = "Writing as a power"
  , cmpText           = "Write as a power "
  , problemStatement  = "Write the following as a power: "
  , context           = showId $ exerciseId powerOfExercise
  }

quadraticExerciseInfo :: MBExerciseInfo
quadraticExerciseInfo = MBExerciseInfo
  { title             = "Solving quadratic equations"
  , cmpText           = "Solve the quadratic equation "
  , problemStatement  = "Solve the following quadratic equation: "
  , context           = showId $ exerciseId quadraticExercise
  }
  
quadraticNoABCExerciseInfo :: MBExerciseInfo
quadraticNoABCExerciseInfo = MBExerciseInfo
  { title             = "Solving quadratic equations (no abc)"
  , cmpText           = "Solve, without using the quadratic formula, the quadratic equation "
  , problemStatement  = "Solve, without using the quadratic formula, the following quadratic equation: "
  , context           = showId $ exerciseId quadraticNoABCExercise
  }
  
quadraticWithApproximationExerciseInfo :: MBExerciseInfo
quadraticWithApproximationExerciseInfo = MBExerciseInfo
  { title             = "Solving quadratic equations (with approximation)"
  , cmpText           = "Solve, with approximation allowed, the quadratic equation "
  , problemStatement  = "Solve, with approximation allowed, the following quadratic equation: "
  , context           = showId $ exerciseId quadraticWithApproximation
  }

rationalEquationExerciseInfo :: MBExerciseInfo
rationalEquationExerciseInfo = MBExerciseInfo
  { title             = "Solving rational equations"
  , cmpText           = "Solve the rational equation "
  , problemStatement  = "Solve the following rational equation: "
  , context           = showId $ exerciseId rationalEquationExercise
  }

simplifyPowerExerciseInfo :: MBExerciseInfo
simplifyPowerExerciseInfo = MBExerciseInfo
  { title             = "Simplifying powers"
  , cmpText           = "Simplify the power "
  , problemStatement  = "Simplify the following power: "
  , context           = showId $ exerciseId simplifyPowerExercise
  }

simplifyRationalExerciseInfo :: MBExerciseInfo
simplifyRationalExerciseInfo = MBExerciseInfo
  { title             = "Simplifying rationals"
  , cmpText           = "Simplify the rational "
  , problemStatement  = "Simplify the following rational: "
  , context           = showId $ exerciseId simplifyRationalExercise
  }

systemWithMatrixExerciseInfo :: MBExerciseInfo
systemWithMatrixExerciseInfo = MBExerciseInfo
  { title             = "Solving systems of linear equations using matrices"
  , cmpText           = "Solve the following system of linear equations using a matrix: "
  , problemStatement  = "Solve the following system of linear equations using a matrix:  "
  , context           = showId $ exerciseId systemWithMatrixExercise
  }

--------------------------------------------------------------------------------
{- XML elements for Omdoc.

Use inefficient string concatenation. 
Probably use Text.XML.
-}
--------------------------------------------------------------------------------

activemathdtd  :: String
activemathdtd  =  "<!DOCTYPE omdoc SYSTEM \"../dtd/activemath.dtd\" []>\n"

cmpelt :: String  -> String -> Element
cmpelt lang cmptext =          
  Element { name        =  "CMP"
          , attributes  =  ["xml:lang" := lang]
          , content     =  [Left cmptext]
          }

creatorelt :: String -> String -> Element
creatorelt role nm =
  Element { name         =  "Creator"
          , attributes   =  ["role" := role ]
          , content      =  [Left nm] 
          }           

dateelt :: String -> String -> Element
dateelt action date =
  Element { name         =  "Date"
          , attributes   =  ["action" := action ]
          , content      =  [Left date] 
          }           

difficultyelt :: String  -> Element
difficultyelt difficulty = 
  Element { name        =  "difficulty"
          , attributes  =  ["value" := difficulty]
          , content     =  []
          }

extradataelt :: [Element] -> Element
extradataelt ls = 
  Element { name        =  "extradata"
          , attributes  =  []
          , content     =  map Right ls
          }

exerciseelt :: String -> [Element] -> Element
exerciseelt idattr ls = 
  Element { name        =  "exercise"
          , attributes  =  ["id" := idattr]
          , content     =  map Right ls
          }

formatelt :: String -> Element
formatelt format = 
  Element { name        =  "Format"
          , attributes  =  []
          , content     =  [Left format]
          }

interaction_generatorelt :: String -> String -> [Element] -> Element
interaction_generatorelt interaction_generatorname interaction_generatortype ls =          
  Element { name        =  "interaction_generator"
          , attributes  =  ["name" := interaction_generatorname
                           ,"type" := interaction_generatortype]
          , content     =  map Right ls
          }

metadataelt :: String -> [Element] -> Element
metadataelt identifier ls = 
  Element { name        =  "metadata"
          , attributes  =  if null identifier
                           then []
                           else ["id" := identifier]
          , content     =  map Right ls
          }

omdocelt :: String -> String -> [Element] -> Element
omdocelt identifier namespace ls =
  Element { name         =  "omdoc"
          , attributes   =  if null namespace 
                            then ["id" := identifier]
                            else if null identifier 
                                 then ["xmlns:ami" := namespace]
                                 else ["id" := identifier
                                      ,"xmlns:ami" := namespace]
          , content      =  map Right ls
          }           

omgroupelt :: String -> String -> [Element] -> Element
omgroupelt identifier namespace ls =
  Element { name         =  "omgroup"
          , attributes   =  if null namespace 
                            then ["id" := identifier]
                            else if null identifier 
                                 then ["xmlns" := namespace]
                                 else ["id" := identifier
                                      ,"xmlns" := namespace]
          , content      =  map Right ls
          }           

omtextelt  :: String -> [Element] -> Element
omtextelt identifier ls =
  Element { name         =  "omtext"
          , attributes   =  ["id" := identifier]
          , content      =  map Right ls
          }           

parameterelt :: String -> Content -> Element
parameterelt parametername ls =          
  Element { name        =  "parameter"
          , attributes  =  ["name" := parametername]
          , content     =  ls
          }

refelt :: String -> String -> Element
refelt xref ami =
  Element { name         =  "ref"
          , attributes   =  ["xref" := xref, "ami:item-element-name" := ami]
          , content      =  []
          }           

sourceelt :: String -> Element
sourceelt source =
  Element { name         =  "Source"
          , attributes   =  []
          , content      =  if null source
                            then []
                            else [Left source]
          }           

sourcefileelt :: String -> String -> String -> Element
sourcefileelt namespace filename lastmodified =
  Element { name         =  "sourcefile"
          , attributes   =  ["xmlns" := namespace
                            ,"path" := filename
                            ,"lastModified" := lastmodified]
          , content      =  []
          }           

theoryelt  :: String -> [Element] -> Element
theoryelt identifier ls =
  Element { name         =  "theory"
          , attributes   =  ["id" := identifier]
          , content      =  map Right ls
          }           

titleelt :: String -> Element
titleelt titletext = 
  Element { name        =  "Title"
          , attributes  =  []
          , content     =  [Left titletext]
          }

xmldecl  :: String
xmldecl  =  "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" 

