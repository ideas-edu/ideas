-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
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
module Domain.Math.Clipboard 
   ( addToClipboard, addListToClipboard
   , lookupClipboard, lookupListClipboard, removeClipboard
     -- generalized interface
   , addToClipboardG, addListToClipboardG
   , lookupClipboardG, lookupListClipboardG
   ) where

import Common.Context
import Control.Monad
import Common.Rewriting
import Data.Maybe
import Domain.Math.Data.Relation
import Domain.Math.Expr
import Text.OpenMath.Object
import qualified Data.Map as M

---------------------------------------------------------------------
-- Expression variables (internal)

newtype ExprVar a = ExprVar (Var OMOBJ)

exprVar :: (Show a, IsTerm a) => String -> a -> ExprVar a
exprVar s a = 
   let omobj = toOMOBJ (toExpr a)
       showF = show . fromOMOBJ
       readF = either (fail . show) (return . toOMOBJ) . parseExpr
   in ExprVar (makeVar showF readF s omobj)

readExprVar :: IsTerm a => ExprVar a -> ContextMonad a
readExprVar (ExprVar var) = do  
   omobj <- readVar var
   maybeCM (fromExpr (fromOMOBJ omobj))

--writeExprVar :: IsExpr a => ExprVar a -> a -> ContextMonad ()
--writeExprVar v = modifyExprVar v . const

modifyExprVar :: IsTerm a => ExprVar a -> (a -> a) -> ContextMonad ()
modifyExprVar (ExprVar var) f = 
   let safe f a = fromMaybe a (f a)
       g = fmap (toOMOBJ . toExpr . f) . fromExpr . fromOMOBJ
   in modifyVar var (safe g)

---------------------------------------------------------------------
-- Clipboard variable

newtype Key = Key String deriving (Show, Eq, Ord)

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

clipboard :: ExprVar (M.Map Key Expr)
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
   modifyExprVar clipboard (M.delete (Key s))

---------------------------------------------------------------------
-- Generalized interface to work with clipboard

addToClipboardG :: IsTerm a => String -> a -> ContextMonad ()
addToClipboardG s a = modifyExprVar clipboard (M.insert (Key s) (toExpr a))

addListToClipboardG :: IsTerm a => [String] -> [a] -> ContextMonad ()
addListToClipboardG = zipWithM_ addToClipboardG

lookupClipboardG :: IsTerm a => String -> ContextMonad a
lookupClipboardG s = do 
   m    <- readExprVar clipboard
   expr <- maybeCM (M.lookup (Key s) m)
   fromExpr expr
   
lookupListClipboardG :: IsTerm a => [String] -> ContextMonad [a]
lookupListClipboardG = mapM lookupClipboardG