-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
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

data Flag = Verbose  | Version | Mode Mode
          | Logging Bool | InputFile String 
 deriving (Show, Eq)

data Mode = XML | JSON | Mixed deriving (Show, Eq)

options :: [OptDescr Flag]
options =
     [ Option ['v']     ["verbose"]    (NoArg Verbose)           "chatty output"
     , Option ['?']     ["version"]    (NoArg Version)           "show version number"
     , Option []        ["xml"]        (NoArg $ Mode XML)        "xml mode"
     , Option []        ["json"]       (NoArg $ Mode JSON)       "json mode"
     , Option ['l']     ["logging"]    (NoArg $ Logging True)    "enable logging (default)"
     , Option []        ["no-logging"] (NoArg $ Logging False)   "disable logging"
     , Option ['f']     ["file"]       (ReqArg InputFile "FILE") "input FILE"
     ]

header :: String
header = "Usage: service [OPTION]      (version " ++ versionNr ++ ")"

versionNr :: String
versionNr = "0.4.1"

serviceOptions :: IO [Flag]
serviceOptions = do
   args <- getArgs
   case getOpt Permute options args of
      (flags, [], []) | Version `notElem` flags ->
         return flags
      (_, _, errs) -> do
         putStrLn (concat errs ++ usageInfo header options)
         exitFailure 

withVerbose :: [Flag] -> Bool
withVerbose flags = Verbose `elem` flags
         
withLogging :: [Flag] -> Bool
withLogging flags = and [ b | Logging b <- flags ]

withMode :: [Flag] -> Mode
withMode flags =
   case (xmlMode, jsonMode) of 
      (True, False) -> XML
      (False, True) -> JSON
      _             -> Mixed
 where
   xmlMode  = Mode XML  `elem` flags
   jsonMode = Mode JSON `elem` flags
   
withInputFile :: [Flag] -> Maybe String
withInputFile flags = 
   case [ file | InputFile file <- flags ] of
      [hd] -> Just hd
      _    -> Nothing