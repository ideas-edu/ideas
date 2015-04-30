-----------------------------------------------------------------------------
-- Copyright 2015, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- The Step datatype defines the symbols during parsing.
--
-----------------------------------------------------------------------------
--  $Id: Core.hs 7590 2015-04-21 07:26:58Z bastiaan $

module Ideas.Common.Strategy.Step
   ( -- * Step
     Step(..), stepRule, stepEnvironment
   ) where

import Ideas.Common.Classes
import Ideas.Common.Environment
import Ideas.Common.Id
import Ideas.Common.Rule
import Ideas.Common.Strategy.Derived

--------------------------------------------------------------------------------
-- Step

-- | The steps during the parsing process: enter (or exit) a labeled
-- sub-strategy, or a rule.
data Step a = Enter Id                      -- ^ Enter a labeled sub-strategy
            | Exit Id                       -- ^ Exit a labeled sub-strategy
            | RuleStep Environment (Rule a) -- ^ Rule that was applied
   deriving Eq

instance Show (Step a) where
   show (Enter l) = "enter " ++ showId l
   show (Exit l)  = "exit " ++ showId l
   show (RuleStep _ r) = show r

instance Apply Step where
   applyAll (RuleStep _ r) = applyAll r
   applyAll _              = return

instance HasId (Step a) where
   getId (Enter l)      = getId l
   getId (Exit l)       = getId l
   getId (RuleStep _ r) = getId r

   changeId f (Enter l)        = Enter (changeId f l)
   changeId f (Exit l)         = Exit  (changeId f l)
   changeId f (RuleStep env r) = RuleStep env (changeId f r)

instance Minor (Step a) where
   setMinor b (RuleStep env r) = RuleStep env (setMinor b r)
   setMinor _ st = st

   isMinor (RuleStep _ r) = isMinor r
   isMinor _ = True

instance AtomicSymbol (Step a) where
   atomicOpen  = RuleStep mempty (idRule "atomic.open")
   atomicClose = RuleStep mempty (idRule "atomic.close")

instance LabelSymbol (Step a) where
   isEnter (Enter _) = True
   isEnter _         = False

stepRule :: Step a -> Rule a
stepRule (RuleStep _ r) = r
stepRule (Enter l)      = idRule (l # "enter")
stepRule (Exit l)       = idRule (l # "exit")

stepEnvironment :: Step a -> Environment
stepEnvironment (RuleStep env _) = env
stepEnvironment _ = mempty