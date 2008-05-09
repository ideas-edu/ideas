{-# OPTIONS -fallow-overlapping-instances -XFlexibleInstances -fallow-undecidable-instances #-}
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
-- (...add description...)
--
-----------------------------------------------------------------------------
module Main (main) where

import Domain.Logic
import Common.Exercise hiding (SyntaxError)
import Common.Context
import Service.JSON
import Service.AbstractService
import System.Environment
import Control.Monad
import Data.Char
import qualified Data.Map as M

main :: IO ()
main = do
   args <- getArgs
   case args of
      [] -> jsonRPCOverHTTP myHandler
      ["--file", filename] -> do
         input <- readFile filename
         txt <- jsonRPC input myHandler
         putStrLn txt
      _ -> do
         putStrLn "service  (use json-rpc to post a request)"
         putStrLn "   --file filename   (to read the request from file)"
     
class Service a where
   service :: a -> [JSON] -> IO JSON
     
instance InJSON a => Service a where
   service a xs = do
      unless (null xs) (fail "Too many arguments for service")
      return (toJSON a)

instance InJSON a => Service (IO a) where
   service m xs = do
      unless (null xs) (fail "Too many arguments for service")
      a <- m
      return (toJSON a)
   
instance (InJSON a, Service b) => Service (a -> b) where
   service f (x:xs) =
      case fromJSON x of
         Just b -> service (f b) xs
         _      -> fail "Invalid argument"
   service _ _ = fail "Not enough arguments for service"
     
serviceTable :: M.Map String ([JSON] -> IO JSON)
serviceTable = M.fromList
   [ ("stepsremaining", service stepsremaining)
   , ("ready",          service ready)
   , ("apply",          service apply)
   , ("applicable",     service applicable)
   , ("onefirst",       service onefirst)
   , ("allfirsts",      service allfirsts)
   , ("derivation",     service derivation)
   , ("generate",       service generate)
   , ("submit",         service submit)
   ]
    
myHandler :: JSON_RPC_Handler
myHandler fun args = 
   case M.lookup fun serviceTable of
      Just f  -> f args
      Nothing -> fail $ "Unknown function: " ++ fun

instance InJSON a => InJSON (Context a) where
   toJSON   = toJSON . fromContext
   fromJSON = fmap inContext . fromJSON

instance InJSON Location where
   toJSON              = toJSON . show
   fromJSON (String s) = case reads s of
                            [(loc, rest)] | all isSpace rest -> Just loc
                            _ -> Nothing
   fromJSON _          = Nothing

instance InJSON Result where
   toJSON result = Object $
      case result of
         SyntaxError   -> [("result", String "SyntaxError")]
         Buggy rs      -> [("result", String "Buggy"), ("rules", Array $ map String rs)]
         NotEquivalent -> [("result", String "NotEquivalent")]   
         Ok rs st      -> [("result", String "Ok"), ("rules", Array $ map String rs), ("state", toJSON st)]
         Detour rs st  -> [("result", String "Detour"), ("rules", Array $ map String rs), ("state", toJSON st)]
         Unknown st    -> [("result", String "Unknown"), ("state", toJSON st)]
   fromJSON (Object xs) = do
      mj <- lookup "result" xs
      ms <- fromString mj
      let getRules = (lookup "rules" xs >>= fromJSON) :: Maybe [RuleID]
          getState = (lookup "state" xs >>= fromJSON) :: Maybe State
      case ms of
         "syntaxerror"   -> return SyntaxError
         "buggy"         -> liftM  Buggy getRules
         "notequivalent" -> return NotEquivalent
         "ok"            -> liftM2 Ok getRules getState
         "detour"        -> liftM2 Detour getRules getState
         "unkown"        -> liftM  Unknown getState
   fromJSON _ = Nothing

fromString :: JSON -> Maybe String
fromString (String s) = Just (map toLower s)
fromString _          = Nothing