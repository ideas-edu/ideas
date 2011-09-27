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
-- Main module for generating Math-Bridge exercises from the ideas framework
--
-----------------------------------------------------------------------------
module Main(main) where

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
import Text.XML

import qualified Main.Revision as MR
import Main.IDEAS

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

lastChanged, version, created, author :: String
lastChanged  =  MR.lastChanged
version      =  MR.version
created      =  "2011-01-22"
author       =  "Johan Jeuring"

revision :: Int
revision     =  MR.revision


--------------------------------------------------------------------------------
{- Generate all required files
-}
--------------------------------------------------------------------------------

dontIncludeExercises  :: [String]
dontIncludeExercises  =  ["logic.propositional.dnf"
                         ,"logic.propositional.dnf.unicode"
                         ,"relationalgebra.cnf"
                         ]

generate :: String -> IO ()
generate dir =
      do allExercises <- useIDEAS getExercises
         let exercises = filter (\(Some ex) -> 
                                     not (show (exerciseId ex) `elem` dontIncludeExercises)) 
                                allExercises
         forM_ exercises $ \(Some ex) -> omdocexercisefileB dir ex
         recbookfileB dir (mapM_ (\(Some ex) -> omdocexerciserefsB ex) exercises)

--------------------------------------------------------------------------------
{- Generating the recbook for the exercise collections
-}
--------------------------------------------------------------------------------

recbookfileB :: String -> XMLBuilder -> IO ()
recbookfileB dir exercisesrefs = do
  let filestring =
             xmldecl
          ++ activemathdtd
          ++ showXML
               (omdocB
                  ""
                  "http://www.activemath.org/namespaces/am_internal"
                  $ do 
                    omgroupB "" "http://www.mathweb.org/omdoc"
                      (omgroupB "IdeasExercises" ""
                         $ do  metadataB "IdeasExercises-metadata"
                                 $ do titlesB titleIdeasExercisesMetadataLangs 
                                              titleIdeasExercisesMetadata
                               omgroupB "recbook_for_IdeasExercises" ""
                                 $ do metadataB ""
                                        $ do titlesB titlesRecbookForIdeasExercisesLangs
                                                     titlesRecbookForIdeasExercises
                                             dateB "created" created
                                             dateB "changed" lastChanged
                                             element "Creator" (("role" .=."aut") >> text author)
                                             sourceB ""
                                             formatB "application/omdoc+xml"
                                      exercisesrefs
                     ) 
               )
  writeFile (dir ++ "omdoc/"  ++ "RecBook_Exercises.omdoc" ) filestring
  writeFile (dir ++ "oqmath/" ++ "RecBook_Exercises.oqmath") filestring

omdocrefpath  :: String
omdocrefpath  =  ""

omdocexerciserefsB :: Exercise a -> XMLBuilder
omdocexerciserefsB ex =
  let info             = mBExerciseInfo ! (exerciseId ex)
      langs            = langSupported info
      titleCmps        = map (\l -> title info l) langs
      len              = length (examples ex)
      refs             = map (\i -> omdocrefpath  -- relative dir 
                                ++  context info  -- filename
                                ++  "/"
                                ++  context info  -- exercise id
                                ++  show i)       -- and nr
                             [0..len-1]
      exercises        = mapM_ refB refs
  in omgroupB "" "" $ do metadataB "" $ do titleMultLangB langs titleCmps 
                         exercises

--------------------------------------------------------------------------------
{- Generating an omdoc file for an exercise
-}
--------------------------------------------------------------------------------

omdocexercisefileB :: String -> Exercise a -> IO ()
omdocexercisefileB dir ex = do
  let info = mBExerciseInfo ! (exerciseId ex)
  let langs = langSupported info
  let filestring =
             xmldecl
          ++ activemathdtd
          ++ showXML
               (omdocB (context info ++ ".omdoc") []
                  $ do metadataB
                         ""
                         (  dateB "created" "2011-01-22" 
                         >> dateB "changed" lastChanged 
                         >> titlesB langs (title info)
                         >> element "Creator" (("role" .=."aut") >> text author)
                         >> versionB version (show revision)
                         ) 
                       theoryB (context info) $ do omdocexercisesB ex
               )
  writeFile (dir ++ "omdoc/"  ++ context info ++ ".omdoc" ) filestring
  writeFile (dir ++ "oqmath/" ++ context info ++ ".oqmath") filestring

omdocexercisesB :: Exercise a -> XMLBuilder
omdocexercisesB ex = zipWithM_ make [(0::Int)..] (examples ex)
 where
   info = mBExerciseInfo ! (exerciseId ex)
   langs = langSupported info
   make nr (dif, example) = do omobj <- toOpenMath ex example
                               let xmlobj = omobj2xml omobj
                               makeElement xmlobj
    where
      makeElement omobj = 
         omdocexerciseB
            (context info ++ show nr)
            (titlesB langs (title info))
            (if null (for info) then Nothing else Just (for info))
            (show dif)
            mblangs
            (map (\l -> (if l `elem` langs then cmp info l else cmp info mbdefaultlang, omobj)) mblangs)
            "IDEASGenerator"
            "strategy"
            (problemStatement info)
            (context info)
            (unescaped $ showXML omobj)

omdocexerciseB :: String
               -> XMLBuilder
               -> Maybe String
               -> String
               -> [Lang]
               -> [(String, XML)]
               -> String
               -> String
               -> String
               -> String
               -> XMLBuilder
               -> XMLBuilder
omdocexerciseB
    exerciseid
    titles
    maybefor
    dif
    langs
    cmps
    interaction_generatorname
    interaction_generatortype
    problemstatement
    ctxt
    task
  = exerciseB
      exerciseid
      Nothing
      (metadataB
         ""
         (titles >>
          formatB "AMEL1.0" >>
          extradataB 
            (difficultyB dif >>
             when (isJust maybefor) (relationB (fromJust maybefor))
            )
         ) >>
       cmpsTaskB langs cmps >>
       interaction_generatorB
         interaction_generatorname
         interaction_generatortype
         (   parameterB "problemstatement" (text problemstatement)
         >>  parameterB "context"          (text ctxt)
         >>  parameterB "difficulty"       (text dif)
         >>  parameterB "task"             task
         )
       )

--------------------------------------------------------------------------------
{- Create a Map for the exercise info
-}
--------------------------------------------------------------------------------

mBExerciseInfo :: Map Id MBExerciseInfo
mBExerciseInfo =
    insertExercise balanceExercise                   balanceExerciseInfo
  $ insertExercise calcPowerExercise                 calcPowerExerciseInfo
  $ insertExercise coverUpExercise                   coverUpExerciseInfo
  $ insertExercise derivativeExercise                derivativeExerciseInfo
  $ insertExercise derivativePolyExercise            derivativePolyExerciseInfo
  $ insertExercise derivativeProductExercise         derivativeProductExerciseInfo
  $ insertExercise derivativeQuotientExercise        derivativeQuotientExerciseInfo
  $ insertExercise expandExercise                    expandExerciseInfo
  $ insertExercise expEqExercise                     expEqExerciseInfo
  $ insertExercise findFactorsExercise               findFactorsExerciseInfo
  $ insertExercise fractionExercise                  fractionExerciseInfo
  $ insertExercise gaussianElimExercise              gaussianElimExerciseInfo
  $ insertExercise gramSchmidtExercise               gramSchmidtExerciseInfo
  $ insertExercise higherDegreeExercise              higherDegreeExerciseInfo
  $ insertExercise ineqHigherDegreeExercise          ineqHigherDegreeExerciseInfo
  $ insertExercise ineqLinearExercise                ineqLinearExerciseInfo
  $ insertExercise ineqQuadraticExercise             ineqQuadraticExerciseInfo
  $ insertExercise linearExercise                    linearExerciseInfo
  $ insertExercise linearMixedExercise               linearMixedExerciseInfo
  $ insertExercise linearSystemExercise              linearSystemExerciseInfo
  $ insertExercise logEqExercise                     logEqExerciseInfo
  $ insertExercise nonNegBrokenExpExercise            nonNegBrokenExpExerciseInfo
  $ insertExercise powerOfExercise                    powerOfExerciseInfo
  $ insertExercise powerEqExercise                    powerEqExerciseInfo
  $ insertExercise quadraticExercise                  quadraticExerciseInfo
  $ insertExercise quadraticNoABCExercise             quadraticNoABCExerciseInfo
  $ insertExercise quadraticWithApproximationExercise quadraticWithApproximationExerciseInfo
  $ insertExercise rationalEquationExercise           rationalEquationExerciseInfo
  $ insertExercise simplifyPowerExercise              simplifyPowerExerciseInfo
  $ insertExercise simplifyRationalExercise           simplifyRationalExerciseInfo
  $ insertExercise systemWithMatrixExercise           systemWithMatrixExerciseInfo
  $ empty
  
insertExercise ex exinfo = let idex = exerciseId ex in insert idex (exinfo idex)  
--------------------------------------------------------------------------------
{- XML elements for Omdoc.
-}
--------------------------------------------------------------------------------

activemathdtd  :: String
activemathdtd  =  "<!DOCTYPE omdoc SYSTEM \"../dtd/activemath.dtd\" []>\n"

cmpsTaskB  :: [Lang] -> [(String,XML)] -> XMLBuilder
cmpsTaskB  =  zipWithM_ (\l s -> cmpTaskB l s)

cmpsTitleB  :: [Lang] -> [String] -> XMLBuilder
cmpsTitleB  =  zipWithM_ (\l s -> cmpTitleB l s)

cmpTaskB :: Lang -> (String,XML) -> XMLBuilder
cmpTaskB lang (taskcmp,omo) =
  element "CMP" (("xml:lang" .=. show lang) >> text taskcmp >> builder omo)

cmpTitleB :: Lang -> String -> XMLBuilder
cmpTitleB lang titlecmp =
  element "CMP" (("xml:lang" .=. show lang) >> text titlecmp)

dateB :: String -> String -> XMLBuilder
dateB action date = element "Date" (("action" .=. action) >> text date)

difficultyB :: String -> XMLBuilder
difficultyB dif = 
  element "difficulty" ("value" .=. dif)

exerciseB :: String -> Maybe String -> XMLBuilder -> XMLBuilder
exerciseB idattr maybefor ls = 
  element "exercise" $ do
    ("id" .=. idattr)
    unless (isNothing maybefor) ("for" .=. fromJust maybefor)
    ls
       
extradataB :: XMLBuilder -> XMLBuilder
extradataB ls = element "extradata" $ do ls

formatB :: String -> XMLBuilder
formatB format = element "Format" (text format)
 
interaction_generatorB :: String -> String -> XMLBuilder -> XMLBuilder
interaction_generatorB interaction_generatorname interaction_generatortype ls = 
  element "interaction_generator" $ do
    ("name" .=. interaction_generatorname)
    ("type" .=. interaction_generatortype)
    ls

metadataB :: String -> XMLBuilder -> XMLBuilder
metadataB identifier ls =
  element "metadata" $ do
    unless (null identifier) ("id" .=. identifier)
    ls

omdocB :: String -> String -> XMLBuilder -> XML
omdocB identifier namespace ls = makeXML "omdoc" $
  do when (not (null identifier)) ("id" .=. identifier)
     when (not (null namespace)) ("xmlns:ami" .=. namespace)
     ls

omgroupB :: String -> String -> XMLBuilder -> XMLBuilder
omgroupB identifier namespace ls =
  element "omgroup" $ do 
    unless (null namespace) ("xmlns" .=. namespace)
    unless (null identifier) ("id" .=. identifier)
    ls

parameterB :: String -> XMLBuilder -> XMLBuilder
parameterB parametername ls =
  element "parameter" $ do
    ("name" .=. parametername)
    ls

relationB :: String -> XMLBuilder
relationB relfor =
  element "relation" $ do
    ("type" .=. "for")
    refB relfor

sourceB :: String -> XMLBuilder
sourceB source = 
  element "Source" $ do unless (null source) (text source)

theoryB :: String -> XMLBuilder -> XMLBuilder
theoryB identifier ls = 
  element "theory" (("id" .=. identifier) >> ls)

titlelangB :: Lang -> String -> XMLBuilder
titlelangB lang titletext =
  element "Title" $ do 
     "xml:lang" .=. show lang
     text titletext

titlesB :: [Lang] -> (Lang -> String) -> XMLBuilder
titlesB langs flang = mapM_ (\l -> titlelangB l (flang l)) langs

titleMultLangB :: [Lang] -> [String] -> XMLBuilder
titleMultLangB langs texts =
  element "Title" $ do cmpsTitleB langs texts

versionB :: String -> String -> XMLBuilder
versionB rev ver = 
  element "Version" (("number" .=. rev) >> text ver)

xmldecl  :: String
xmldecl  =  "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"

refB :: String -> XMLBuilder
refB xref = do element "ref" ("xref" .=. xref)
     
     