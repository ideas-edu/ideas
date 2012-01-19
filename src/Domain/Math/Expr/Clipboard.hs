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
   ( -- * Data type
     Clipboard 
     -- * Interface
   , addToClipboard, removeClipboard, lookupClipboard
     -- * Generalized interface
   , addToClipboardG, lookupClipboardG
   ) where

import Common.Library
import Control.Monad
import Data.Maybe
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

instance Reference Clipboard

clipboard :: Ref Clipboard
clipboard = makeRef "clipboard"

getClipboard :: Context a -> Clipboard
getClipboard = fromMaybe (C M.empty) . (clipboard ?)

changeClipboard :: (Clipboard -> Clipboard) -> Context a -> Context a
changeClipboard f c = insertRef clipboard (f (getClipboard c)) c

---------------------------------------------------------------------
-- Interface to work with clipboard

addToClipboard :: String -> Expr -> Context a -> Context a
addToClipboard = addToClipboardG

lookupClipboard :: String -> Context b -> Maybe Expr
lookupClipboard = lookupClipboardG

removeClipboard :: String -> Context a -> Context a
removeClipboard s = changeClipboard (C . M.delete s . unC)

---------------------------------------------------------------------
-- Generalized interface to work with clipboard

addToClipboardG :: IsTerm a => String -> a -> Context b -> Context b
addToClipboardG s a = changeClipboard (C . M.insert s (toExpr a) . unC)

lookupClipboardG :: IsTerm a => String -> Context b -> Maybe a
lookupClipboardG s c = clipboard ? c >>= M.lookup s . unC >>= fromExpr