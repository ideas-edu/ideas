-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
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
module Main.Options where

import Data.Maybe
import Documentation.Make
import Main.LoggingDatabase (logEnabled)
import Main.Revision
import System.Console.GetOpt
import System.Environment
import System.Exit

data Flag = Version | Help | Logging Bool | InputFile String 
          | FixRNG | DocItem Documentation
   deriving Eq

header :: String
header = 
   "IDEAS: Intelligent Domain-specific Exercise Assistants\n" ++
   "Copyright 2010, Open Universiteit Nederland\n" ++
   versionText ++
   "\n\nUsage: ideas [OPTION]     (by default, CGI protocol)\n" ++
   "\nOptions:"

versionText :: String
versionText = 
  "version " ++ version ++ ", revision " ++ show revision ++
  ", logging " ++ (if logEnabled then "enabled" else "disabled")

fullVersion :: String
fullVersion = "version " ++ version ++ " (revision " 
           ++ show revision ++ ", " ++ lastChanged ++ ")"

shortVersion :: String
shortVersion = version ++ " (" ++ show revision ++ ")"

options :: [OptDescr Flag]
options =
     [ Option []  ["version"]    (NoArg Version)           "show version number"
     , Option "?" ["help"]       (NoArg Help)              "show options"
     , Option "l" ["logging"]    (NoArg $ Logging True)    "enable logging"
     , Option []  ["no-logging"] (NoArg $ Logging False)   "disable logging (default on local machine)"
     , Option "f" ["file"]       (ReqArg InputFile "FILE") "use input FILE as request"
     , Option ""  ["fixed-rng"]  (NoArg FixRNG)            "use a fixed random-number generator"
     , Option ""  ["make-pages"] (docItemDescr (Pages      . fromMaybe "docs")) "generate pages for exercises and services"
     , Option ""  ["make-rules"] (docItemDescr (LatexRules . fromMaybe "docs")) "generate latex code for rewrite rules"
     , Option ""  ["self-check"] (docItemDescr (SelfCheck  . fromMaybe "test")) "perform a self-check"
     ]

docItemDescr :: (Maybe String -> Documentation) -> ArgDescr Flag
docItemDescr f = OptArg (DocItem . f) "DIR"

serviceOptions :: IO [Flag]
serviceOptions = do
   args <- getArgs
   case getOpt Permute options args of
      (flags, [], [])
         | flags == [Version] -> do 
              putStrLn ("IDEAS, " ++ versionText)
              exitSuccess
         | all (`notElem` flags) [Version, Help] ->
              return flags
      (_, _, errs) -> do
         putStrLn (concat errs ++ usageInfo header options)
         exitFailure 

docItems :: [Flag] -> [Documentation]
docItems flags = [ x | DocItem x <- flags ]

documentationMode :: [Flag] -> Bool
documentationMode = not . null . docItems

withLogging :: [Flag] -> Bool
withLogging flags = and [ b | Logging b <- flags ]
   
withInputFile :: [Flag] -> Maybe String
withInputFile flags = 
   case [ file | InputFile file <- flags ] of
      [hd] -> Just hd
      _    -> Nothing