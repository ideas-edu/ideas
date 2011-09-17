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