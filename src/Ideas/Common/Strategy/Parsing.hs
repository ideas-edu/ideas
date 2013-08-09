-----------------------------------------------------------------------------
-- Copyright 2013, Open Universiteit Nederland. This file is distributed
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
module Ideas.Common.Strategy.Parsing
   ( Step(..)
   , ParseState, makeState, choices, trace
   , parseDerivationTree, replay, runCore, indepState, toProcess
   ) where

import Data.Function (on)
import Data.Monoid
import Ideas.Common.Utils (fst3)
import Ideas.Common.Classes
import Ideas.Common.DerivationTree
import Ideas.Common.Environment
import Ideas.Common.Rule
import Ideas.Common.Strategy.Core
import qualified Ideas.Common.Strategy.Sequential as Sequential
import Ideas.Common.Strategy.Sequential hiding (replay)

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
makeState a = S [] emptyPath . newProcess a . withPath . toProcess

----------------------------------------------------------------------
-- Parse derivation tree

parseDerivationTree :: a -> ParseState l a -> DerivationTree (Step l a) (a, ParseState l a)
parseDerivationTree = curry (makeTree next)
 where
   next (_, st) = (empty (remainder st), stateFirsts st (remainder st))
   
   stateFirsts st p = 
      [ ( step
        , (a, st {trace = step:trace st, remainder = q, choices = path})
        )
      | ((step, a, path), q) <- Sequential.firsts p
      ]

indepState :: (Step l a -> Bool) -> (Step l a -> Step l a -> Bool) -> ParseState l a -> ParseState l a
indepState p eq state = 
    state { remainder = uniquePath (p . fst3) (eq `on` fst3) (remainder state) }

----------------------------------------------------------------------
-- Running the parser

runCore :: Core l a -> a -> [a]
runCore = runProcess . toProcess . noLabels 
 where
   runProcess p a = rec a (filterMin2 (applyMin2 a p))

   rec a p = 
      (if empty p then (a:) else id)
      [ c
      | ((_, b), q) <- firsts p
      , c <- rec b q
      ]
      
-----------------------------

toProcess :: Core l a -> Process (Step l a)
toProcess = fromAtoms . build . rec . coreSubstAll
 where
   rec core = 
      case core of
         a :*: b   -> rec a <*> rec b
         a :|: b   -> rec a <|> rec b
         Rule r    -> single (Single (RuleStep mempty r))
         a :|>: b  -> rec a <?> rec b
         Fail      -> stop
         Succeed   -> ok
         Label l a -> Single (Enter l) ~> rec a 
                      <*> single (Single (Exit l))
         a :%: b   -> concurrent switch (build (rec a)) (build (rec b))
         a :@: b   -> build (rec a) <@> build (rec b)
         Atomic a  -> atomic (build (rec a))
         Rec _ _   -> error "toMin: rec"
         Var _     -> error "toMin: var"

   switch (Single (Enter _)) = False
   switch _ = True
   
applyMin2 :: a -> Process (Step l a) -> Process (Step l a, a)
applyMin2 = scanChoice step
 where
   step a (RuleStep _ r) =
      [ (b, (RuleStep env r, b))
      | (b, env) <- transApply (transformation r) a
      ]
   step a st = [(a, (st, a))]

applyMin :: a -> Process (Step l a, Path) -> Process (Step l a, a, Path)
applyMin = scanChoice step
 where
   step a (RuleStep _ r, bs) =
      [ (b, (RuleStep env r, b, bs))
      | (b, env) <- transApply (transformation r) a
      ]
   step a (st, bs) = [(a, (st, a, bs))]
 
filterMin2 :: Process (Step l a, a) -> Process (Step l a, a)
filterMin2 = prune (isMajor . fst)
 
-- remove left-biased choice
filterMin :: Process (Step l a, a, Path) -> Process (Step l a, a, Path)
filterMin = prune (\(st, _, _) -> isMajor st)

replay :: Monad m => Path -> a -> Core l a -> m (ParseState l a)
replay path a core = do 
   (as, p) <- Sequential.replay path $ withPath $ toProcess core
   return (S (map fst as) path (newProcess a p))
      
newProcess :: a -> Process (Step l a, Path) -> Process (Step l a, a, Path)
newProcess a core = filterMin (applyMin a core)