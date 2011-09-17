module Options where

import Data.Char
import System.Console.GetOpt
import System.Environment
import System.Exit

data Flag = Mode Mode | Output Output | Export Export 
          | OutputAll | Limit Int | Stats | Save
   deriving Eq

data Mode = Report | HTML
   deriving Eq

data Output = SystemSize | PackageSize | ModuleSize | FunctionSize 
            | Imports | ImportsOf
   deriving (Eq, Enum)

data Export = ExportLOC (Maybe FilePath) | ExportImports (Maybe FilePath)
            | ImportGraph (Maybe FilePath)
   deriving Eq

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
     [ option "report" (mode Report) "report mode (default)"
     , option "html"   (mode HTML)   "write html"
     , Option "s" ["system-size"]   (output SystemSize)    "system size"
     , Option "p" ["package-size"]  (output PackageSize)   "package size"
     , Option "m" ["module-size"]   (output ModuleSize)    "module size"
     , Option "f" ["function-size"] (output FunctionSize)  "function size"
     , option "all"           (NoArg OutputAll)      "all reports"
     , option "imports"       (output Imports)       "#imports in module"
     , option "imports-of"    (output ImportsOf)     "#imports of module"
     , option "limit"         limitArg               "limit number of rows"
     , option "stats"         (NoArg Stats)          "show statistics"
     , option "save"          (NoArg Save)           "save information to file"
     , option "ex-loc"        (export ExportLOC)     "export lines of code"
     , option "ex-imports"    (export ExportImports) "export module imports"
     , option "import-graph"  (export ImportGraph)   "export the import graph"
     ]
 where
   option = Option "" . return
   mode   = NoArg . Mode
   output = NoArg . Output
   export f = OptArg (Export . f) "FILE"
   
limitArg :: ArgDescr Flag
limitArg = flip ReqArg "INT" $ \s ->
   if all isDigit s && not (null s) 
   then Limit (read s)
   else error $ "Invalid argument: --limit=<INT>"

readOptions :: IO ([Flag], [FilePath])
readOptions = do
   args <- getArgs
   case getOpt Permute options args of
      (flags, inputs, errs)
         | null errs && not (null inputs) -> 
              return (flags, inputs)
         | otherwise -> do
              putStrLn (concat errs ++ usageInfo header options)
              exitFailure

modes :: [Flag] -> [Mode]
modes flags =
   case [ a | Mode a <- flags ] of
      [] -> [Report]
      xs -> xs

outputs :: [Flag] -> [Output]
outputs flags 
   | OutputAll `elem` flags = [SystemSize .. ImportsOf]
   | otherwise =
        case [ a | Output a <- flags]  of
           [] -> [SystemSize]
           xs -> xs

exports :: [Flag] -> [Export]
exports flags = [ a | Export a <- flags ]

showStatistics :: [Flag] -> Bool
showStatistics = elem Stats