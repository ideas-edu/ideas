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
-- Refinement rules for the programming domain.
--
---------------------------------------------------------------------------

module Domain.Programming.HeliumCoreInstances where

import Core
import IdMap
import IntMap

deriving instance Show Expr
deriving instance Show Note
deriving instance Show Binds
deriving instance Show Bind
deriving instance Show Alt
deriving instance Show Pat
deriving instance Show Literal
deriving instance Show a => Show (Con a) 
deriving instance Show a => Show (Module a)
deriving instance Show a => Show (Decl a)
deriving instance Show Custom
deriving instance Show Access

instance Show (IdMap a) where
  show _ = ""
instance Show (IntMap a) where
  show _ = ""
