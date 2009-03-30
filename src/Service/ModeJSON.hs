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
import Common.Utils (Some(..))
import Common.Exercise (Exercise(..), ExerciseCode, exerciseCode)
import Common.Transformation (name, isBuggyRule, isRewriteRule)
import Service.JSON
import Service.AbstractService
import qualified Service.ExerciseList as List
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
   , ("onefirsttext",   service1 onefirsttext)
   , ("allfirsts",      service1 allfirsts)
   , ("derivation",     service1 derivation)
   , ("generate",       service2IO generate)
   , ("submit",         service2 submit)
   , ("submittext",     service2 submittext)
   , ("exerciselist",   exerciseList)
   , ("rulelist",       ruleList)
   ]

exerciseList :: [JSON] -> IO JSON
exerciseList (_:_) = fail "Too many arguments for service"
exerciseList _ = return $ Array $ map f List.exerciseList
 where
   f (Some ex) = Object
      [ ("domain",      String $ domain ex)
      , ("identifier",  String $ identifier ex)
      , ("description", String $ description ex)
      , ("status",      String $ show $ status ex)
      ]

ruleList :: [JSON] -> IO JSON
ruleList [String code] =
   case filter p List.exerciseList of
      [Some ex] -> return $ Array $ map f (ruleset ex)
      _ -> fail "unknown exercise code"
 where
   p (Some ex) = show (exerciseCode ex) == code
   f r = Object 
      [ ("name",        String  $ name r)
      , ("buggy",       Boolean $ isBuggyRule r) 
      , ("rewriterule", Boolean $ isRewriteRule r)
      ]
ruleList _ = fail "ruleList service requires exactly one argument (with a string)"
 
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
   toJSON = toJSON . fromContext
   fromJSON a = fromJSON a >>= (return . inContext)

instance InJSON ExerciseCode where 
   toJSON = toJSON . show
   fromJSON (String s) = List.resolveExerciseCode s
   fromJSON _          = fail "expecting a string"
   
instance InJSON Location where
   toJSON              = toJSON . show
   fromJSON (String s) = case reads s of
                            [(loc, rest)] | all isSpace rest -> return loc
                            _ -> fail "invalid string"
   fromJSON _          = fail "expecting a string"

instance InJSON Result where
   toJSON result = Object $
      case result of
         SyntaxError _ -> [("result", String "SyntaxError")]
         Buggy rs      -> [("result", String "Buggy"), ("rules", Array $ map String rs)]
         NotEquivalent -> [("result", String "NotEquivalent")]   
         Ok rs st      -> [("result", String "Ok"), ("rules", Array $ map String rs), ("state", toJSON st)]
         Detour rs st  -> [("result", String "Detour"), ("rules", Array $ map String rs), ("state", toJSON st)]
         Unknown st    -> [("result", String "Unknown"), ("state", toJSON st)]
   fromJSON (Object xs) = do
      mj <- lookupM "result" xs
      ms <- fromString mj
      let getRules = (lookupM "rules" xs >>= fromJSON)
          getState = (lookupM "state" xs >>= fromJSON)
      case ms of
         -- "syntaxerror"   -> return SyntaxError
         "buggy"         -> liftM  Buggy getRules
         "notequivalent" -> return NotEquivalent
         "ok"            -> liftM2 Ok getRules getState
         "detour"        -> liftM2 Detour getRules getState
         "unkown"        -> liftM  Unknown getState
         _               -> fail "Unknown service" 
   fromJSON _ = fail "expecting an object"

fromString :: Monad m => JSON -> m String
fromString (String s) = return (map toLower s)
fromString _          = fail "expecting a string"

-- copied from JSON module
lookupM :: Monad m => String -> [(String, a)] -> m a
lookupM x = maybe (fail $ "field " ++ x ++ " not found") return . lookup x