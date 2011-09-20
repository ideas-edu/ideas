module Main(main) where

-- document buggy rules for AM
-- second ``for'' ref?
-- feedbacktexts
-- notify partners of two new exercises
-- overhaul XML generation

import ExerciseInfo
import Languages

import Domain.LinearAlgebra
import Domain.Math.Derivative.Exercises
import Domain.Math.Equation.CoverUpExercise
import Domain.Math.Numeric.Exercises
import Domain.Math.Polynomial.Balance
import Domain.Math.Polynomial.Exercises
import Domain.Math.Polynomial.IneqExercises
import Domain.Math.Polynomial.RationalExercises
import Domain.Math.Power.Equation.Exercises
import Domain.Math.Power.Exercises

import Common.Library
import Common.Utils

import Service.OpenMathSupport
import Service.DomainReasoner

import Text.OpenMath.Object
import Text.XML.Interface
import Text.XML

import qualified Main.Revision as MR
import Main.IDEAS

-- import Data.Map((!))
import Data.Maybe
import Data.Map((!),Map,empty,insert)


import System.Environment
import Control.Monad

--------------------------------------------------------------------------------
{- main
-}
--------------------------------------------------------------------------------

main :: IO ()
main = do
   args <- getArgs
   case args of
      [dir] -> generate dir
      _     -> putStrLn "Omdoc <output directory>" 

--------------------------------------------------------------------------------
{- Global info
-}
--------------------------------------------------------------------------------

lastChanged, version :: String
lastChanged  =  MR.lastChanged
version      =  MR.version

revision :: Int
revision     =  MR.revision

--------------------------------------------------------------------------------
{- Generate all required files
-}
--------------------------------------------------------------------------------

dontIncludeExercises = ["logic.propositional.dnf"
                       ,"logic.propositional.dnf.unicode"
                       ,"relationalgebra.cnf"
                       ,"algebra.manipulation.polynomial.expand"
                       ]

generate :: String -> IO ()
generate dir =
      do allExercises <- useIDEAS getExercises
         let exercises = filter (\(Some ex) -> not (show (exerciseId ex) `elem` dontIncludeExercises)) allExercises
         forM_ exercises $ \(Some ex) -> 
           omdocexercisefile dir ex
         recbookfile 
           dir 
           [omdocexerciserefs ex | Some ex <- exercises] 

--------------------------------------------------------------------------------
{- Generating the recbook for the exercise collections
-}
--------------------------------------------------------------------------------

recbookfile :: String -> [Element] -> IO ()
recbookfile dir exercisesrefs = do
  let filestring =
             xmldecl
          ++ activemathdtd
          ++ showXML
               (omdocelt
                  ""
                  "http://www.activemath.org/namespaces/am_internal"
                  [omgroupelt "" "http://www.mathweb.org/omdoc"
                    [omgroupelt "IdeasExercises" ""
                      [metadataelt "IdeasExercises-metadata"
                        (titleelts [EN,NL] ["Ideas Exercises collection","Ideas opgaven"])
                      ,omgroupelt "recbook_for_IdeasExercises" ""
                        (metadataelt ""
                          (titleelts [EN,NL] ["Complete Ideas Exercises Recbook","Recbook voor alle Ideas opgaven"]
                           ++
                           [dateelt "created" "2011-01-22"
                           ,dateelt "changed" lastChanged
                           ,creatorelt "aut" "Johan Jeuring"
                           ,sourceelt ""
                           ,formatelt "application/omdoc+xml"
                           ]
                          )
                        :exercisesrefs
                        )
                      ]
                    ]
                  ]
               )
  writeFile (dir ++ "omdoc/"  ++ "RecBook_Exercises.omdoc" ) filestring
  writeFile (dir ++ "oqmath/" ++ "RecBook_Exercises.oqmath") filestring

omdocrefpath  :: String
omdocrefpath  =  ""

omdocexerciserefs :: Exercise a -> Element
omdocexerciserefs ex =
  let info             = mBExerciseInfo ! (exerciseId ex)
      langs            = langSupported info
      titleCmps        =  map (\l -> title info l) langs
      len              = length (examples ex)
      refs             = map (\i -> omdocrefpath  -- relative dir (the same right now)
                                ++  context info  -- filename
                                ++  "/"
                                ++  context info  -- exercise id
                                ++  show i)       -- and nr
                             [0..len-1]
      exercises        = map xrefelt refs
  in omgroupelt "" "" (metadataelt "" [titleeltMultLang langs titleCmps]:exercises)

--------------------------------------------------------------------------------
{- Generating an omdoc file for an exercise
-}
--------------------------------------------------------------------------------

omdocexercisefile :: String -> Exercise a -> IO ()
omdocexercisefile dir ex = do
  let info = mBExerciseInfo ! (exerciseId ex)
  let langs = langSupported info
  let titleTexts =  map (\l -> title info l) langs
  let filestring =
             xmldecl
          ++ activemathdtd
          ++ showXML
               (omdocelt
                  (context info ++ ".omdoc")
                  []
                  [metadataelt
                    ""
                    ([dateelt "created" "2011-01-22"
                     ,dateelt "changed" lastChanged
                     ] ++
                     titleelts langs titleTexts ++
                     [creatorelt "aut" "Johan Jeuring"
                     ,versionelt version (show revision)
                     ]
                    )
                  ,theoryelt (context info)
                             (omdocexercises ex)
                  ]
               )
  writeFile (dir ++ "omdoc/"  ++ context info ++ ".omdoc" ) filestring
  writeFile (dir ++ "oqmath/" ++ context info ++ ".oqmath") filestring

omdocexercises :: {- (IsTerm a) => -} Exercise a -> [Element]
omdocexercises ex = catMaybes $ zipWith make [(0::Int)..] (examples ex)
 where
   info = mBExerciseInfo ! (exerciseId ex)
   langs = langSupported info
   titleTexts =  map (\l -> title info l) langs
   make nr (dif, example) =
      fmap (makeElement . omobj2xml) (toOpenMath ex example)
    where
      makeElement omobj =
         omdocexercise
            (context info ++ show nr)
            (titleelts langs titleTexts)
            (if null (for info) then Nothing else Just (for info))
            (show dif)
            mblangs
            (map (\l -> (if l `elem` langs then cmp info l else cmp info mbdefaultlang) ++ show omobj) mblangs)
            "IDEASGenerator"
            "strategy"
            (problemStatement info)
            (context info)
            omobj

omdocexercise :: String
              -> [Element]
              -> Maybe String
              -> String
              -> [Lang]
              -> [String]
              -> String
              -> String
              -> String
              -> String
              -> Element
              -> Element
omdocexercise
    exerciseid
    titles
    maybefor
    difficulty
    langs
    cmps
    interaction_generatorname
    interaction_generatortype
    problemstatement
    ctxt
    task
  = exerciseelt
      exerciseid
      Nothing
      ([metadataelt
         ""
         (titles ++
          [formatelt "AMEL1.0"
          ,extradataelt
            (difficultyelt difficulty
            :case maybefor of
               Just for -> [relationelt for]
               Nothing  -> []
            )
          ]
         )
       ]
       ++
       cmpelts langs cmps
       ++
       [interaction_generatorelt
         interaction_generatorname
         interaction_generatortype
         [parameterelt "problemstatement" [Left problemstatement]
         ,parameterelt "context"          [Left ctxt]
         ,parameterelt "difficulty"       [Left difficulty]
         ,parameterelt "task"             [Right task]
         ]
       ]
      )

mBExerciseInfo :: Map Id MBExerciseInfo
mBExerciseInfo =
  let balanceExerciseId             = exerciseId balanceExercise
      calcPowerExerciseId           = exerciseId calcPowerExercise
      coverUpExerciseId             = exerciseId coverUpExercise
      derivativeExerciseId          = exerciseId derivativeExercise
      derivativePolyExerciseId      = exerciseId derivativePolyExercise
      derivativeProductExerciseId   = exerciseId derivativeProductExercise
      derivativeQuotientExerciseId  = exerciseId derivativeQuotientExercise
      expandExerciseId              = exerciseId expandExercise
      expEqExerciseId               = exerciseId expEqExercise
      findFactorsExerciseId         = exerciseId findFactorsExercise
      fractionExerciseId            = exerciseId fractionExercise
      gaussianElimExerciseId        = exerciseId gaussianElimExercise
      gramSchmidtExerciseId         = exerciseId gramSchmidtExercise
      higherDegreeExerciseId        = exerciseId higherDegreeExercise
      ineqHigherDegreeExerciseId    = exerciseId ineqHigherDegreeExercise
      ineqLinearExerciseId          = exerciseId ineqLinearExercise
      ineqQuadraticExerciseId       = exerciseId ineqQuadraticExercise
      linearExerciseId              = exerciseId linearExercise
      linearMixedExerciseId         = exerciseId linearMixedExercise
      linearSystemExerciseId        = exerciseId linearSystemExercise
      logEqExerciseId               = exerciseId logEqExercise
      nonNegBrokenExpExerciseId     = exerciseId nonNegBrokenExpExercise
      powerOfExerciseId             = exerciseId powerOfExercise
      powerEqExerciseId             = exerciseId powerEqExercise
      quadraticExerciseId           = exerciseId quadraticExercise
      quadraticNoABCExerciseId      = exerciseId quadraticNoABCExercise
      quadraticWithApproximationId  = exerciseId quadraticWithApproximation
      rationalEquationExerciseId    = exerciseId rationalEquationExercise
      simplifyPowerExerciseId       = exerciseId simplifyPowerExercise
      simplifyRationalExerciseId    = exerciseId simplifyRationalExercise
      systemWithMatrixExerciseId    = exerciseId systemWithMatrixExercise
  in  insert balanceExerciseId            (balanceExerciseInfo                    balanceExerciseId)
    $ insert calcPowerExerciseId          (calcPowerExerciseInfo                  calcPowerExerciseId)
    $ insert coverUpExerciseId            (coverUpExerciseInfo                    coverUpExerciseId)
    $ insert derivativeExerciseId         (derivativeExerciseInfo                 derivativeExerciseId)
    $ insert derivativePolyExerciseId     (derivativePolyExerciseInfo             derivativePolyExerciseId)
    $ insert derivativeProductExerciseId  (derivativeProductExerciseInfo          derivativeProductExerciseId)
    $ insert derivativeQuotientExerciseId (derivativeQuotientExerciseInfo         derivativeQuotientExerciseId)
    $ insert expandExerciseId             (expandExerciseInfo                     expandExerciseId)
    $ insert expEqExerciseId              (expEqExerciseInfo                      expEqExerciseId)
    $ insert findFactorsExerciseId        (findFactorsExerciseInfo                findFactorsExerciseId)
    $ insert fractionExerciseId           (fractionExerciseInfo                   fractionExerciseId)
    $ insert gaussianElimExerciseId       (gaussianElimExerciseInfo               gaussianElimExerciseId)
    $ insert gramSchmidtExerciseId        (gramSchmidtExerciseInfo                gramSchmidtExerciseId)
    $ insert higherDegreeExerciseId       (higherDegreeExerciseInfo               higherDegreeExerciseId)
    $ insert ineqHigherDegreeExerciseId   (ineqHigherDegreeExerciseInfo           ineqHigherDegreeExerciseId)
    $ insert ineqLinearExerciseId         (ineqLinearExerciseInfo                 ineqLinearExerciseId)
    $ insert ineqQuadraticExerciseId      (ineqQuadraticExerciseInfo              ineqQuadraticExerciseId)
    $ insert linearExerciseId             (linearExerciseInfo                     linearExerciseId)
    $ insert linearMixedExerciseId        (linearMixedExerciseInfo                linearMixedExerciseId)
    $ insert linearSystemExerciseId       (linearSystemExerciseInfo               linearSystemExerciseId)
    $ insert logEqExerciseId              (logEqExerciseInfo                      logEqExerciseId)
    $ insert nonNegBrokenExpExerciseId    (nonNegBrokenExpExerciseInfo            nonNegBrokenExpExerciseId)
    $ insert powerOfExerciseId            (powerOfExerciseInfo                    powerOfExerciseId)
    $ insert powerEqExerciseId            (powerEqExerciseInfo                    powerEqExerciseId)
    $ insert quadraticExerciseId          (quadraticExerciseInfo                  quadraticExerciseId)
    $ insert quadraticNoABCExerciseId     (quadraticNoABCExerciseInfo             quadraticNoABCExerciseId)
    $ insert quadraticWithApproximationId (quadraticWithApproximationExerciseInfo quadraticWithApproximationId)
    $ insert rationalEquationExerciseId   (rationalEquationExerciseInfo           rationalEquationExerciseId)
    $ insert simplifyPowerExerciseId      (simplifyPowerExerciseInfo              simplifyPowerExerciseId)
    $ insert simplifyRationalExerciseId   (simplifyRationalExerciseInfo           simplifyRationalExerciseId)
    $ insert systemWithMatrixExerciseId   (systemWithMatrixExerciseInfo           systemWithMatrixExerciseId)
      empty

--------------------------------------------------------------------------------
{- XML elements for Omdoc.

Use inefficient string concatenation.
Probably use Text.XML.
-}
--------------------------------------------------------------------------------

activemathdtd  :: String
activemathdtd  =  "<!DOCTYPE omdoc SYSTEM \"../dtd/activemath.dtd\" []>\n"

cmpelts :: [Lang] -> [String] -> [Element]
cmpelts = zipWith (\l s -> cmpelt (show l) s)

cmpelt :: String  -> String -> Element
cmpelt lang cmp =
  Element { name        =  "CMP"
          , attributes  =  ["xml:lang" := lang]
          , content     =  [Left cmp]
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

exerciseelt :: String -> Maybe String -> [Element] -> Element
exerciseelt idattr maybefor ls =
  case maybefor of
    Nothing  -> Element { name        =  "exercise"
                        , attributes  =  ["id" := idattr]
                        , content     =  map Right ls
                        }
    Just for -> Element { name        =  "exercise"
                        , attributes  =  ["id" := idattr, "for" := for]
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
          , attributes   = if null namespace && null identifier
                           then []
                           else if null namespace
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

relationelt :: String -> Element
relationelt for =
  Element { name         =  "relation"
          , attributes   =  ["type" := "for"]
          , content      =  [Right (xrefelt for)]
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

titleeltlang :: Lang -> String -> Element
titleeltlang lang titletext =
  Element { name        =  "Title"
          , attributes  =  ["xml:lang" := show lang]
          , content     =  [Left titletext]
          }

titleelts :: [Lang] -> [String] -> [Element]
titleelts langs texts = zipWith titleeltlang langs texts

titleeltMultLang :: [Lang] -> [String] -> Element
titleeltMultLang langs texts =
  Element { name        =  "Title"
          , attributes  =  []
          , content     =  map Right (cmpelts langs texts)
          }

versionelt :: String -> String -> Element
versionelt version revision =
  Element { name        =  "Version"
          , attributes  =  ["number" := revision]
          , content     =  [Left version]
          }

xmldecl  :: String
xmldecl  =  "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"

xrefelt :: String -> Element
xrefelt xref =
  Element { name         =  "ref"
          , attributes   =  ["xref" := xref]
          , content      =  []
          }