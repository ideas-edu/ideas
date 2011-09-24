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

-- document buggy rules for AM
-- feedbacktexts
-- notify partners of two new exercises

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
                                             creatorB "aut"  author
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
--  let titleTexts =  map (\l -> title info l) langs
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
                         >> creatorB "aut" "Johan Jeuring" 
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
            (map (\l -> (if l `elem` langs then cmp info l else cmp info mbdefaultlang, unescaped $ showXML omobj)) mblangs)
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
               -> [(String, XMLBuilder)]
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
-}
--------------------------------------------------------------------------------

activemathdtd  :: String
activemathdtd  =  "<!DOCTYPE omdoc SYSTEM \"../dtd/activemath.dtd\" []>\n"

cmpsTaskB  :: [Lang] -> [(String,XMLBuilder)] -> XMLBuilder
cmpsTaskB  =  zipWithM_ (\l s -> cmpTaskB l s)

cmpsTitleB  :: [Lang] -> [String] -> XMLBuilder
cmpsTitleB  =  zipWithM_ (\l s -> cmpTitleB l s)

cmpTaskB :: Lang -> (String,XMLBuilder) -> XMLBuilder
cmpTaskB lang (taskcmp,omo) =
  element "CMP" (("xml:lang" .=. show lang) >> text taskcmp >> omo)

cmpTitleB :: Lang -> String -> XMLBuilder
cmpTitleB lang titlecmp =
  element "CMP" (("xml:lang" .=. show lang) >> text titlecmp)

creatorB :: String -> String -> XMLBuilder
creatorB role nm = 
  element "Creator" (("role" .=. role) >> text nm)

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
extradataB ls = 
  element "extradata" $ do ls

formatB :: String -> XMLBuilder
formatB format = 
  element "Format" (text format)
 
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
     
     