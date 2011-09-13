module Options where

import Data.Char
import System.Console.GetOpt
import System.Environment
import System.Exit

data Flag = Input String | Output Output | OutputAll | Limit Int | Stats | Save
   deriving Eq

data Output = SystemSize | PackageSize | ModuleSize | FunctionSize 
            | Imports | ImportsOf
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
     [ Option "s" ["system-size"]   (output SystemSize)   "system size"
     , Option "p" ["package-size"]  (output PackageSize)  "package size"
     , Option "m" ["module-size"]   (output ModuleSize)   "module size"
     , Option "f" ["function-size"] (output FunctionSize) "function size"
     , Option ""  ["all"]           (NoArg OutputAll)     "all reports"
     , Option ""  ["imports"]       (output Imports)      "#imports in module"
     , Option ""  ["imports-of"]    (output ImportsOf)    "#imports of module"
     , Option ""  ["limit"]         limitArg              "limit number of rows"
     , Option ""  ["stats"]         (NoArg Stats)         "show statistics"
     , Option ""  ["save"]          (NoArg Save)          "save information to file"
     ]
 where
   output = NoArg . Output
   
limitArg :: ArgDescr Flag
limitArg = flip ReqArg "<INT>" $ \s ->
   if all isDigit s && not (null s) 
   then Limit (read s)
   else error $ "Invalid argument: --limit=<INT>"
   
getFlags :: IO [Flag]
getFlags = do
   args <- getArgs
   case getOpt Permute options args of
      (flags, xs, errs)
         | null errs && not (null xs) -> 
              return $ flags ++ map Input xs
         | otherwise -> do
              putStrLn (concat errs ++ usageInfo header options)
              exitFailure
              
inputs :: [Flag] -> [String]
inputs flags = [ s | Input s <- flags ]

outputs :: [Flag] -> [Output]
outputs flags
   | OutputAll `elem` flags = [SystemSize .. ImportsOf]
   | null xs   = [SystemSize]
   | otherwise = xs
 where
   xs = [ a | Output a <- flags ]
   
showStatistics :: [Flag] -> Bool
showStatistics = elem Stats