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
     
-- copy/paste starts here :-(
myHandler :: JSON_RPC_Handler
myHandler fun args = 
   case fun of
      "stepsremaining" -> 
         case args of 
            [Array [String exid, String prefix, String expr]] ->
               let n = stepsremaining (exid, prefix, inContext expr) 
               in return (toJSON n, True)
            _  -> return (String $ "Invalid arguments for function " ++ fun, False) 
      "ready" -> 
         case args of 
            [Array [String exid, String prefix, String expr]] ->
               let b = ready (exid, prefix, inContext expr)
               in return (toJSON b, True)
            _  -> return (String $ "Invalid arguments for function " ++ fun, False) 
      "apply" -> 
         case args of 
            [String ruleid, String loc, Array [String exid, String prefix, String expr]] ->
               let b = apply ruleid (read loc) (exid, prefix, inContext expr)
               in return (stateToJSON b, True)
            _  -> return (String $ "Invalid arguments for function " ++ fun, False) 
      "applicable" -> 
         case args of 
            [String loc, Array [String exid, String prefix, String expr]] ->
               let b = applicable (read loc) (exid, prefix, inContext expr)
               in return (listRuleIDToJSON b, True)
            _  -> return (String $ "Invalid arguments for function " ++ fun, False) 
      "onefirst" -> 
         case args of 
            [Array [String exid, String prefix, String expr]] ->
               let b = onefirst (exid, prefix, inContext expr)
               in return (tripleToJSON b, True)
            _  -> return (String $ "Invalid arguments for function " ++ fun, False) 
      "allfirsts" -> 
         case args of 
            [Array [String exid, String prefix, String expr]] ->
               let b = allfirsts (exid, prefix, inContext expr)
               in return (Array $ map tripleToJSON b, True)
            _  -> return (String $ "Invalid arguments for function " ++ fun, False) 
      "derivation" -> 
         case args of 
            [Array [String exid, String prefix, String expr]] ->
               let b = derivation (exid, prefix, inContext expr)
               in return (Array $ map triple2ToJSON b, True)
            _  -> return (String $ "Invalid arguments for function " ++ fun, False) 
      "generate" -> 
         case args of 
            [Array [String exid, Number (I level)]] ->
               let s = generate exid (fromInteger level)
               in return (stateToJSON s, True)
            _  -> return (String $ "Invalid arguments for function " ++ fun, False) 
      "submit" -> 
         case args of 
            [Array [String exid, String prefix, String expr], String e] ->
               let a = submit (exid, prefix, inContext expr) e
               in return (resultToJSON a, True)
            _  -> return (String $ "Invalid arguments for function " ++ fun, False) 
      _ -> return (String $ "Unknown function: " ++ fun, False)
      
stateToJSON :: State -> JSON
stateToJSON (a, b, c) = Array [String a, String b, String $ fromContext c]

listRuleIDToJSON :: [RuleID] -> JSON
listRuleIDToJSON = Array . map String

tripleToJSON :: (RuleID, Location, State) -> JSON
tripleToJSON (a, b, c) = Array [String a, String (show b), stateToJSON c]

triple2ToJSON :: (RuleID, Location, Expression) -> JSON
triple2ToJSON (a, b, c) = Array [String a, String (show b), String c]

resultToJSON :: Result -> JSON
resultToJSON result = 
   case result of
      SyntaxError   -> Object [("result", String "SyntaxError")]
      Buggy rs      -> Object [("result", String "Buggy"), ("rules", Array $ map String rs)]
      NotEquivalent -> Object [("result", String "NotEquivalent")]   
      Ok rs st      -> Object [("result", String "Ok"), ("rules", Array $ map String rs), ("state", stateToJSON st)]
      Detour rs st  -> Object [("result", String "Detour"), ("rules", Array $ map String rs), ("state", stateToJSON st)]
      Unknown st    -> Object [("result", String "Unknown"), ("state", stateToJSON st)]