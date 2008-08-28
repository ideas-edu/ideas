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
-- Services using JSON notation
--
-----------------------------------------------------------------------------
module Service.ModeJSON (processJSON) where

import Common.Context
import Service.JSON
import Service.AbstractService
import qualified Data.Map as M
import Control.Monad
import Data.Char

processJSON :: String -> IO (String, String)
processJSON input = do
   txt <- jsonRPC input myHandler
   return (txt, "application/json")
   
myHandler :: JSON_RPC_Handler
myHandler fun arg = 
   case (M.lookup fun serviceTable, arg) of
      (Just f, Array args) -> f args
      (Just _, _) -> fail $ "The params part is not an object"
      _ -> fail $ "Unknown function: " ++ fun
      
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
   
type Service a = a -> [JSON] -> IO JSON
   
service :: InJSON a => Service a
service a xs = do
   unless (null xs) (fail "Too many arguments for service")
   return (toJSON a)
     
fun :: InJSON a => Service b -> Service (a -> b)
fun srv = \f args ->
   case args of 
      x:xs ->
         case fromJSON x of
            Just b -> srv (f b) xs
            _      -> fail "Invalid argument"
      _ -> fail $ "Argument not found for service"

io :: InJSON a => Service a -> Service (IO a)
io srv ma xs = do
   a <- ma
   srv a xs

service1   f = fun service f
service2   f = fun service1 f
service3   f = fun service2 f
service2IO f = fun (fun (io service)) f

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
         _               -> Nothing
   fromJSON _ = Nothing

fromString :: JSON -> Maybe String
fromString (String s) = Just (map toLower s)
fromString _          = Nothing