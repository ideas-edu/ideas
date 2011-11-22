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
   ( addToClipboard, addListToClipboard
   , lookupClipboard, lookupListClipboard, removeClipboard
     -- generalized interface
   , addToClipboardG, addListToClipboardG
   , lookupClipboardG, lookupListClipboardG
   , maybeOnClipboardG
   ) where

import Common.Argument
import Common.Context
import Common.Rewriting
import Control.Monad
import Data.Typeable
import Domain.Math.Data.Relation
import Domain.Math.Expr.Data
import Domain.Math.Expr.Parser
import qualified Data.Map as M

---------------------------------------------------------------------
-- Expression variables (internal)

exprVar :: (Typeable a, IsTerm a) => String -> a -> ArgDescr a
exprVar s a = (emptyArgDescr s a (show . toExpr))
   { parseArgument    = join . liftM fromExpr . parseExprM
   , termViewArgument = Just termView 
   }

---------------------------------------------------------------------
-- Clipboard variable

newtype Key = Key String deriving (Show, Eq, Ord, Typeable)

instance (IsTerm k, Ord k, IsTerm a) => IsTerm (M.Map k a) where
   toTerm = toTerm . map (\(k, a) -> toTerm k :==: toTerm a) . M.toList
   fromTerm term = do
      eqs <- fromTerm term
      xs  <- forM eqs $ \(a :==: b) ->
                liftM2 (,) (fromTerm a) (fromTerm b)
      return (M.fromList xs)

instance IsTerm Key where
   toTerm (Key s) = variable s
   fromTerm       = liftM Key . getVariable

clipboard :: ArgDescr (M.Map Key Expr)
clipboard = exprVar "clipboard" M.empty

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
removeClipboard s =
   modifyVar clipboard (M.delete (Key s))

---------------------------------------------------------------------
-- Generalized interface to work with clipboard

addToClipboardG :: IsTerm a => String -> a -> ContextMonad ()
addToClipboardG s a = modifyVar clipboard (M.insert (Key s) (toExpr a))

addListToClipboardG :: IsTerm a => [String] -> [a] -> ContextMonad ()
addListToClipboardG = zipWithM_ addToClipboardG

lookupClipboardG :: IsTerm a => String -> ContextMonad a
lookupClipboardG s = do
   m    <- readVar clipboard
   expr <- maybeCM (M.lookup (Key s) m)
   fromExpr expr

maybeOnClipboardG :: IsTerm a => String -> ContextMonad (Maybe a)
maybeOnClipboardG s = do
   m <- readVar clipboard
   return (M.lookup (Key s) m >>= fromExpr)

lookupListClipboardG :: IsTerm a => [String] -> ContextMonad [a]
lookupListClipboardG = mapM lookupClipboardG