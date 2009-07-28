module Domain.Programming.Prog where

import Common.Context
import Common.Grammar
import Common.Strategy
import Data.Generics.Biplate
import Data.Generics.PlateData
import Data.Data hiding (Fixity)
import Data.List hiding (union, lookup)
import Domain.Programming.AlphaConv (alphaConversion)
import Domain.Programming.Strategies (getRules)
import Domain.Programming.Helium


-- Test values
(Right m) = compile "f x = x + 3"

-- Some help functions
collectNames :: Module -> [String]
collectNames m = nub [ s | Name_Identifier _ _ s <- universeBi m ]

equalModules :: Module -> Module -> Bool
equalModules x y = f x == f y
  where
    f = alphaConversion . removeRanges
    
removeRanges = transformBi (\(Range_Range  _ _) -> noRange)

-- allSolutions strat = map (fromContext . snd . last . snd) $ derivations strat $ inContext emptyProg
isSolution strat m = member m (noLabels strat)

checkExercise :: Strategy (Context Module) -> String -> IO ()
checkExercise strat x = putStrLn $ x ++ " : " ++ show (isSolution strat (getRules (fromRight (compile x))))
  where
    fromRight x = case x of
                    Right y -> y
                    _       -> error "no compile"
checkExercises :: Strategy (Context Module) -> [String] -> IO ()
checkExercises strat xs = mapM_ (checkExercise strat) xs

