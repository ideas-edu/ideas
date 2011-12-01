{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Support for a clipboard, on which expressions can be placed. The clipboard
-- is part of the environment (terms that are placed in a context)
--
-----------------------------------------------------------------------------
module Domain.Math.Expr.Clipboard
   ( Clipboard 
   , addToClipboard, addListToClipboard
   , lookupClipboard, lookupListClipboard, removeClipboard
     -- generalized interface
   , addToClipboardG, addListToClipboardG
   , lookupClipboardG, lookupListClipboardG
   , lookupClipboardIn
   , maybeOnClipboardG
   ) where

import Common.Library
import Common.Results
import Control.Monad
import Data.Typeable
import Domain.Math.Data.Relation
import Domain.Math.Expr.Data
import Domain.Math.Expr.Parser
import qualified Data.Map as M

---------------------------------------------------------------------
-- Clipboard variable

newtype Clipboard = C {unC :: M.Map String Expr}
   deriving Typeable

instance Show Clipboard where
   show = show . toExpr

instance Read Clipboard where
   readsPrec _ txt = do
      expr <- parseExprM txt
      clip <- fromExpr expr
      return (clip, "")
      
instance IsTerm Clipboard where
   toTerm = 
      let f (s, a) = Var s :==: a
      in toTerm . map f . M.toList . unC
   fromTerm = 
      let f (x :==: a) = liftM (\k -> (k, a)) (getVariable x)
      in liftM (C . M.fromList) . mapM f . fromTerm

clipboard :: Binding Clipboard
clipboard = makeBindingWith (C M.empty) "clipboard"

---------------------------------------------------------------------
-- Interface to work with clipboard

addToClipboard :: String -> Expr -> Results ()
addToClipboard = addToClipboardG

addListToClipboard :: [String] -> [Expr] -> Results ()
addListToClipboard = addListToClipboardG

lookupClipboard :: String -> Results Expr
lookupClipboard = lookupClipboardG

lookupListClipboard :: [String] -> Results [Expr]
lookupListClipboard = lookupListClipboardG

removeClipboard :: String -> Results ()
removeClipboard s = modifyVar clipboard (C . M.delete s . unC)

---------------------------------------------------------------------
-- Generalized interface to work with clipboard

addToClipboardG :: IsTerm a => String -> a -> Results ()
addToClipboardG s a = modifyVar clipboard (C . M.insert s (toExpr a) . unC)

addListToClipboardG :: IsTerm a => [String] -> [a] -> Results ()
addListToClipboardG = zipWithM_ addToClipboardG

lookupClipboardG :: IsTerm a => String -> Results a
lookupClipboardG s = do
   m    <- readVar clipboard
   expr <- toResults (M.lookup s (unC m))
   fromExpr expr

lookupClipboardIn :: IsTerm a => String -> Environment -> Maybe a
lookupClipboardIn s env = do 
   clip <- lookupValue (getId clipboard) env
   expr <- M.lookup s (unC clip)
   fromExpr expr

maybeOnClipboardG :: IsTerm a => String -> Results (Maybe a)
maybeOnClipboardG s = do
   m <- readVar clipboard
   return (M.lookup s (unC m) >>= fromExpr)

lookupListClipboardG :: IsTerm a => [String] -> Results [a]
lookupListClipboardG = mapM lookupClipboardG