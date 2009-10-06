-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Options and command-line flags for services
--
-----------------------------------------------------------------------------
module Service.Options where

import System.Environment
import System.Exit
import System.Console.GetOpt
import Service.Revision (version, revision)
import Service.LoggingDatabase (logEnabled)

data Flag = Verbose | Version | Logging Bool | InputFile String 
 deriving (Show, Eq)

options :: [OptDescr Flag]
options =
     [ Option "?"     ["version"]    (NoArg Version)           "show version number"
     , Option "l"     ["logging"]    (NoArg $ Logging True)    "enable logging"
     , Option []      ["no-logging"] (NoArg $ Logging False)   "disable logging (default on local machine)"
     , Option "f"     ["file"]       (ReqArg InputFile "FILE") "input FILE"
     ]

header :: String
header = "Usage: service [OPTION]   (" ++ versionText ++ ", logging " ++ 
         (if logEnabled then "enabled" else "disabled") ++ ")"

versionText :: String
versionText = "version " ++ version ++ ", revision " ++ show revision

serviceOptions :: IO [Flag]
serviceOptions = do
   args <- getArgs
   case getOpt Permute options args of
      (flags, [], []) | Version `notElem` flags ->
         return flags
      (_, _, errs) -> do
         putStrLn (concat errs ++ usageInfo header options)
         exitFailure 
         
withLogging :: [Flag] -> Bool
withLogging flags = and [ b | Logging b <- flags ]
   
withInputFile :: [Flag] -> Maybe String
withInputFile flags = 
   case [ file | InputFile file <- flags ] of
      [hd] -> Just hd
      _    -> Nothing