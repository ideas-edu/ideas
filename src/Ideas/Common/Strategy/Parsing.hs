-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Basic machinery for executing a core strategy expression.
--
-----------------------------------------------------------------------------
--  $Id$

module Ideas.Common.Strategy.Parsing
   ( Step(..)
   , ParseState, makeState, choices, trace
   , parseDerivationTree, replay, runCore, searchModeState
   ) where

import Data.Function (on)
import Data.Monoid
import Ideas.Common.Classes hiding (singleton)
import Ideas.Common.DerivationTree
import Ideas.Common.Environment
import Ideas.Common.Rule
import Ideas.Common.Strategy.Core
import Ideas.Common.Strategy.Path
import Ideas.Common.Strategy.Process
import Ideas.Common.Utils (fst3)
import Ideas.Common.Strategy.Derived
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Sequence hiding (Step)

----------------------------------------------------------------------
-- Step data type

data Step l a = Enter l | Exit l | RuleStep Environment (Rule a)
   deriving (Eq)

instance Show (Step l a) where
   show (Enter _) = "Enter"
   show (Exit _)  = "Exit"
   show (RuleStep _ r) = show r

-- A core expression where the symbols are steps instead of "only" rules
-- type StepCore l a = GCore l (Step l a)

instance Apply (Step l) where
   applyAll (RuleStep _ r) = applyAll r
   applyAll _              = return

instance Minor (Step l a) where
   setMinor b (RuleStep env r) = RuleStep env (setMinor b r)
   setMinor _ step = step

   isMinor (RuleStep _ r) = isMinor r
   isMinor _ = True

----------------------------------------------------------------------
-- State data type

data ParseState l a = S
   { trace     :: [Step l a]
   , choices   :: Path
   , remainder :: Process (Step l a, a, Path)
   }

makeState :: a -> Core l a -> ParseState l a
makeState a = S [] emptyPath . applyMin a . withPath . coreToProcess

----------------------------------------------------------------------
-- Parse derivation tree

parseDerivationTree :: a -> ParseState l a -> DerivationTree (Step l a) (a, ParseState l a)
parseDerivationTree = curry (makeTree next)
 where
   next (_, st) = (ready (remainder st), stateFirsts st (remainder st))

   stateFirsts st p =
      [ ( step
        , (a, st {trace = step:trace st, remainder = q, choices = path})
        )
      | ((step, a, path), q) <- firsts p
      ]

searchModeState :: (Step l a -> Bool) -> (Step l a -> Step l a -> Bool) -> ParseState l a -> ParseState l a
searchModeState p eq state =
    state { remainder = -- tidyProcess eq' (not . p') $
                        uniquePath p' eq' (remainder state) }
  where
    eq' = eq `on` fst3
    p'  = p . fst3

----------------------------------------------------------------------
-- Running the parser

runCore :: Core l a -> a -> [a]
runCore = runProcess . coreToProcess . noLabels
 where
   runProcess p a = bests (accum applyAll a p)

-----------------------------

coreToProcess :: Core l a -> Process (Step l a)
coreToProcess = fromAtoms . toProcess . rec . coreSubstAll
 where
   rec :: Core l a -> Builder (Sym (Step l a))
   rec core =
      case core of
         a :*: b   -> rec a <*> rec b
         a :|: b   -> rec a <|> rec b
         Rule r    -> single (Single (RuleStep mempty r))
         a :|>: b  -> rec a |> rec b
         Fail      -> empty
         Succeed   -> done
         Label l a -> Single (Enter l) ~> rec a
                      <*> single (Single (Exit l))
         a :%: b   -> concurrent switch (rec a) (rec b)
         a :@: b   -> rec a <@> rec b
         Atomic a  -> atomic (rec a)
         Let _ _   -> error "toMin: let"
         Var _     -> error "toMin: var"

   switch (Single (Enter _)) = False
   switch _ = True

applyMin :: a -> Process (Step l a, Path) -> Process (Step l a, a, Path)
applyMin a0 = prune (isMajor . fst3) . scan step a0
 where
   step a (RuleStep _ r, bs) =
      [ (b, (RuleStep env r, b, bs))
      | (b, env) <- transApply (transformation r) a
      ]
   step a (st, bs) = [(a, (st, a, bs))]

replay :: Monad m => Path -> a -> Core l a -> m (ParseState l a)
replay path a core = do
   (as, p) <- replayPath path $ withPath $ coreToProcess core
   return (S (map fst as) path (applyMin a p))