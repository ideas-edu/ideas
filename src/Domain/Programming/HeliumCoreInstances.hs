{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, TypeSynonymInstances #-}

---------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
---------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  unknown
--
---------------------------------------------------------------------------

module Domain.Programming.HeliumCoreInstances where

import Data.Data
import Core
import IdSet

deriving instance Show Expr
deriving instance Show Note
deriving instance Show Binds
deriving instance Show Bind
deriving instance Show Alt
deriving instance Show Pat
deriving instance Show Literal
deriving instance Show a => Show (Con a) 

instance Show IdSet where
  show _ = ""

getExprs :: Module Expr -> [Expr]
getExprs (Module _ _ _ ds) = map (\(DeclValue _ _ _ v _) -> v) ds
