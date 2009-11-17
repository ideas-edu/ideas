-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  unknown
--
-- This module contains the domain-specific functions
-----------------------------------------------------------------------------

module Domain.Programming.Prog where

import Common.Derivation
import Common.Strategy hiding (not, repeat, replicate)
import Control.Monad.State
import Data.Generics.Biplate ()
import Data.Generics.PlateData
import Data.List hiding (union, lookup, insert)
import Domain.Programming.Helium
import Domain.Programming.HeliumRules
import Domain.Programming.Transformations

------------------------------------------------------------------------------
-- | Data types
------------------------------------------------------------------------------
data Solution = Solution { solution :: String
                         , sid      :: String
                         , strat    :: LabeledStrategy Module
                         , remark   :: String }

type Solutions = [Solution]


------------------------------------------------------------------------------
-- | Functions to check wheter a student solution is correct.
------------------------------------------------------------------------------
collectNames :: Module -> [String]
collectNames m = nub [ s | Name_Identifier _ _ s <- universeBi m ]

freshVarStrings :: Module -> [String]
freshVarStrings = (['x' : show i | i <- [1..]] \\) . collectNames

allSolutions :: (IsStrategy s, Undefined a) => s a -> [a]
allSolutions strategy = results $ derivationTree strategy undef

normalisedSolutions fs = 
  map (normaliseModule fs) . allSolutions

isEquivalent fs strat m = 
  elem (normaliseModule fs m) $ normalisedSolutions fs strat

isSolution :: IsStrategy s => [String] -> s Module -> Module -> Bool
isSolution fixedNames strat =
  (`elem` normalisedSolutions fixedNames strat) . normaliseModule fixedNames

compile' :: String -> Module
compile' = either (error "Compilation error!") id . compile

printRow :: String -> String -> String -> String -> IO ()
printRow id isCorrect strategyName remark =   
    putStrLn $ pps id ++ pps isCorrect ++ pps strategyName 
            ++ ppStringS 40 remark
  where
    ppStringS i s = "| " ++ s ++ replicate (i - length s) ' '
    pps = ppStringS 20

checkExercise :: [String] -> Solution -> StateT Integer IO ()
checkExercise fixedNames s = do
  correctCount <- get  
  let isCorrect = isSolution fixedNames (strat s) (compile' (solution s))
  liftIO $ printRow (sid s) (show isCorrect) (strategyName (strat s)) (remark s)
  liftIO line
  put $ if isCorrect then correctCount + 1 else correctCount

checkExercises :: [String] -> Solutions -> IO ()
checkExercises fixedNames solutions = do
  when (null fixedNames) $ error "At least one function name should be given"
  line 
  printRow "Identifier" "Is recognised" "By strategy" "Remark"
  putStrLn $ replicate 80 '='
  (_, count) <- runStateT (mapM (checkExercise fixedNames) solutions) 0
  let percentage = take 4 $ show $ count * 100  `div` toInteger (length solutions)
  putStrLn $ "\n" ++ percentage ++ "% has been recognised by the strategy.\n"
  return ()

line  = putStrLn $ replicate 80 '-'

------------------------------------------------------------------------------
-- Test values
------------------------------------------------------------------------------
(Right m') = compile "f x y = x + y"
(Right m2) = compile "f = \\ y -> \\x -> div x y"
(Right m3) = compile "f a b c d = a ; g = ((f 1) 2 3) ; h = g 4"
(Right m3') = compile "f a b c d = a ; g = f 1 2 3 ; t = g 4"
(Right m4) = compile "f x y = x ; g x y = f x y where f x y = y"
(Right m5) = compile "f x y = x ; g x y = f x y"
(Right m6) = compile "f x = g x where g y = reverse y"
(Right m) = compile "f n [] = 0\nf m (_:xs) = m + f m xs\ng = f 2\n"
checkFromBin = checkExercises ["fromBin"]
