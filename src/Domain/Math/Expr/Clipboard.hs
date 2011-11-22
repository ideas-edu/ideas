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
   , maybeOnClipboardG
   ) where

import Common.Library
import Control.Monad
import Domain.Math.Data.Relation
import Domain.Math.Expr.Data
import Domain.Math.Expr.Parser
import qualified Data.Map as M

---------------------------------------------------------------------
-- Clipboard variable

type Clipboard = M.Map String Expr

clipboard :: ArgDescr Clipboard
clipboard = (emptyArgDescr "clipboard" M.empty (show . toExpr . fromMap)) -- TODO: remove toExpr
   { parseArgument    = \txt -> parseExprM txt >>= fromExpr >>= toMap
   , termViewArgument = Just mapView
   }
 where
   mapView :: View Term Clipboard
   mapView = makeView (toMap . fromTerm) (toTerm . fromMap)
 
   fromMap :: Clipboard -> Equations Expr
   fromMap = map (uncurry ((:==:) . Var)) . M.toList
   
   toMap :: Equations Expr -> Maybe Clipboard
   toMap = liftM M.fromList . mapM f
    where
      f (x :==: a) = liftM (\k -> (k, a)) (getVariable x)

---------------------------------------------------------------------
-- Interface to work with clipboard

addToClipboard :: String -> Expr -> ContextMonad ()
addToClipboard = addToClipboardG

addListToClipboard :: [String] -> [Expr] -> ContextMonad ()
addListToClipboard = addListToClipboardG

lookupClipboard :: String -> ContextMonad Expr
lookupClipboard = lookupClipboardG

lookupListClipboard :: [String] -> ContextMonad [Expr]
lookupListClipboard = lookupListClipboardG

removeClipboard :: String -> ContextMonad ()
removeClipboard = modifyVar clipboard . M.delete

---------------------------------------------------------------------
-- Generalized interface to work with clipboard

addToClipboardG :: IsTerm a => String -> a -> ContextMonad ()
addToClipboardG s = modifyVar clipboard . M.insert s . toExpr

addListToClipboardG :: IsTerm a => [String] -> [a] -> ContextMonad ()
addListToClipboardG = zipWithM_ addToClipboardG

lookupClipboardG :: IsTerm a => String -> ContextMonad a
lookupClipboardG s = do
   m    <- readVar clipboard
   expr <- maybeCM (M.lookup s m)
   fromExpr expr

maybeOnClipboardG :: IsTerm a => String -> ContextMonad (Maybe a)
maybeOnClipboardG s = do
   m <- readVar clipboard
   return (M.lookup s m >>= fromExpr)

lookupListClipboardG :: IsTerm a => [String] -> ContextMonad [a]
lookupListClipboardG = mapM lookupClipboardG