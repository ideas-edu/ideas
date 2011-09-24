-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  johan.jeuring@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Module for defining languages used in Math-Bridge
--
-----------------------------------------------------------------------------
module Languages where

--------------------------------------------------------------------------------
{- Different languages can be supported.
-}
--------------------------------------------------------------------------------

data Lang = EN | ES | DE | FR | NL | FI | HU deriving Eq

instance Show Lang where
  show EN = "en"
  show ES = "es"
  show DE = "de"
  show FR = "fr"
  show NL = "nl"
  show FI = "fi"
  show HU = "hu"

mblangs        =  [EN,ES,DE,FR,NL,FI,HU]
mbdefaultlang  =  EN