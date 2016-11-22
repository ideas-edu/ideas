-----------------------------------------------------------------------------
-- Copyright 2016, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Options and command-line flags for services
--
-----------------------------------------------------------------------------

module Ideas.Main.Options
   ( Flag(..), getFlags
   , versionText, helpText, shortVersion, fullVersion
   ) where

import Data.Maybe
import Ideas.Service.Logging (logEnabled)
import Ideas.Main.Revision
import System.Console.GetOpt
import System.Environment
import System.Exit

data Flag = Version | Help | PrintLog
          | InputFile String | Test FilePath
          | MakeScriptFor String | AnalyzeScript FilePath
   deriving Eq

header :: String
header =
   "IDEAS: Intelligent Domain-specific Exercise Assistants\n" ++
   "Copyright 2016, Open Universiteit Nederland\n" ++
   versionText ++
   "\n\nUsage: ideas [OPTION]     (by default, CGI protocol)\n" ++
   "\nOptions:"

versionText :: String
versionText =
  "version " ++ ideasVersion ++ ", revision " ++ ideasRevision ++
  ", logging " ++ (if logEnabled then "enabled" else "disabled")

helpText :: String
helpText = usageInfo header options

fullVersion :: String
fullVersion = "version " ++ ideasVersion ++ " (revision "
           ++ ideasRevision ++ ", " ++ ideasLastChanged ++ ")"

shortVersion :: String
shortVersion = ideasVersion ++ " (" ++ ideasRevision ++ ")"

options :: [OptDescr Flag]
options =
   [ Option []  ["version"]        (NoArg Version)  "show version number"
   , Option "?" ["help"]           (NoArg Help)     "show options"
   , Option ""  ["print-log"]      (NoArg PrintLog) "print log information (for debugging)"
   , Option "f" ["file"]           fileArg          "use input FILE as request"
   , Option ""  ["test"]           testArg          "run tests on directory (default: 'test')"
   , Option ""  ["make-script"]    makeScrArg       "generate feedback script for exercise"
   , Option ""  ["analyze-script"] analyzeScrArg    "analyze feedback script and report errors"
   ]

fileArg, testArg, makeScrArg, analyzeScrArg :: ArgDescr Flag
fileArg       = ReqArg InputFile "FILE"
testArg       = OptArg (Test . fromMaybe "test") "DIR"
makeScrArg    = ReqArg MakeScriptFor "ID"
analyzeScrArg = ReqArg AnalyzeScript "FILE"

getFlags :: IO [Flag]
getFlags = do
   args <- getArgs
   case getOpt Permute options args of
      (flags, [], []) -> return flags
      (_, _, errs) -> do
         putStrLn (concat errs ++ helpText)
         exitFailure