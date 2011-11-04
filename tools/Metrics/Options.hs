module Options 
   ( Flag(..), Output(..), Metric(..)
   , metrics, getOutput, readOptions, outputFile, saveFile, showStatistics
   ) where

import Data.Char
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.Exit

data Flag = Output Output | Metric Metric
          | AllMetrics | Limit Int | Stats 
          | OutputFile FilePath
          | Save FilePath
   deriving Eq

data Output = Report | HTML | JavaScript 
   deriving Eq

data Metric = SystemSize | PackageSize | ModuleSize | FunctionSize 
            | Imports | ImportsOf | ImportGraph
   deriving (Eq, Enum)

header :: String
header =
   "Haskell Metrics\n" ++
   "Copyright 2011, Open Universiteit Nederland\n" ++
   version ++
   "\n\nUsage: hmetrics [OPTION] [INPUT]\n" ++
   "\nOptions:"

version :: String
version = "version 0.1"

options :: [OptDescr Flag]
options =
     [ option "report" (output Report) "report (default)"
     , option "html"   (output HTML)   "write html"
     , option "js"     (output JavaScript) "JavaScript data file"
     , option "save"   saveArg         "save system information (to file)"
     , Option "s" ["system-size"]   (metric SystemSize)    "system size"
     , Option "p" ["package-size"]  (metric PackageSize)   "package size"
     , Option "m" ["module-size"]   (metric ModuleSize)    "module size"
     , Option "f" ["function-size"] (metric FunctionSize)  "function size"
     , option "all"           (NoArg AllMetrics)      "all metrics"
     , option "imports"       (metric Imports)       "#imports in module"
     , option "imports-of"    (metric ImportsOf)     "#imports of module"
     , option "limit"         limitArg               "limit number of rows"
     , option "stats"         (NoArg Stats)          "show statistics"
     , option "output" outputFileArg "write output to file"
     ]
 where
   option = Option "" . return
   output = NoArg . Output
   metric = NoArg . Metric
   
limitArg :: ArgDescr Flag
limitArg = flip ReqArg "INT" $ \s ->
   if all isDigit s && not (null s) 
   then Limit (read s)
   else error $ "Invalid argument: --limit=<INT>"

outputFileArg :: ArgDescr Flag
outputFileArg = ReqArg OutputFile "FILE"

saveArg :: ArgDescr Flag
saveArg = ReqArg Save "FILE"

readOptions :: IO ([Flag], [FilePath])
readOptions = do
   args <- getArgs
   case getOpt Permute options args of
      (flags, inputs, errs)
         | not (null errs) -> do
              putStrLn (concat errs ++ usageInfo header options)
              exitFailure
         | null (inputs) -> do
              putStrLn "No input files/directories"
              exitFailure
         | length (outputs flags) > 1 -> do
              putStrLn "Multiple outputs"
              exitFailure
         | otherwise -> 
              return (flags, inputs)

getOutput :: [Flag] -> Output
getOutput = head . outputs

outputs :: [Flag] -> [Output]
outputs flags =
   case [ a | Output a <- flags ] of
      [] -> [Report]
      xs -> xs

outputFile :: [Flag] -> Maybe FilePath
outputFile flags = listToMaybe $ 
   [ a | OutputFile a <- flags ]

saveFile :: [Flag] -> Maybe FilePath
saveFile flags = listToMaybe $ 
   [ a | Save a <- flags ]

metrics :: [Flag] -> [Metric]
metrics flags 
   | AllMetrics `elem` flags = [SystemSize .. ImportsOf]
   | otherwise =
        case [ a | Metric a <- flags]  of
           [] | isNothing (saveFile flags) -> [SystemSize]
           xs -> xs

showStatistics :: [Flag] -> Bool
showStatistics = elem Stats