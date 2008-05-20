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

type Service a = a -> [JSON] -> IO JSON


service :: InJSON a => Service a
service a xs = do
   unless (null xs) (fail "Too many arguments for service")
   return (toJSON a)
      
serviceIO :: InJSON a => Service (IO a)
serviceIO m xs = do
   unless (null xs) (fail "Too many arguments for service")
   a <- m
   return (toJSON a)

serviceFun :: InJSON a => Service b -> Service (a -> b)
serviceFun srv f (x:xs) =
   case fromJSON x of
      Just b -> srv (f b) xs
      _      -> fail "Invalid argument"
serviceFun _ _ _ = fail "Not enough arguments for service"    

service1 f = serviceFun service f
service2 f = serviceFun (serviceFun service) f
service2IO f = serviceFun (serviceFun serviceIO) f
service3 f = serviceFun (serviceFun (serviceFun service)) f

serviceTable :: M.Map String ([JSON] -> IO JSON)
serviceTable = M.fromList
   [ ("stepsremaining", service1 stepsremaining)
   , ("ready",          service1 ready)
   , ("apply",          service3 apply)
   , ("applicable",     service2 applicable)
   , ("onefirst",       service1 onefirst)
   , ("allfirsts",      service1 allfirsts)
   , ("derivation",     service1 derivation)
   , ("generate",       service2IO generate)
   , ("submit",         service2 submit)
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