{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}

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

import Data.Data
import Core

deriving instance Show Expr
deriving instance Show Note
deriving instance Show Binds
deriving instance Show Bind
deriving instance Show Alt
deriving instance Show Pat
deriving instance Show Literal
deriving instance Show a => Show (Con a) 

deriving instance Data Expr
deriving instance Data Note
deriving instance Data Binds
deriving instance Data Bind
deriving instance Data Alt
deriving instance Data Pat
deriving instance Data Literal
deriving instance Data a => Data (Con a)

deriving instance Typeable Expr
deriving instance Typeable Note
deriving instance Typeable Binds
deriving instance Typeable Bind
deriving instance Typeable Alt
deriving instance Typeable Pat
deriving instance Typeable Literal

