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
   , ParseState, makeState, majorOnly, getPath, trace
   , ready, firsts, firstsWith
   , replay, runCore, searchModeState
   ) where

import Data.Maybe
import Data.Function (on)
import Data.Monoid
import Ideas.Common.Classes hiding (singleton)
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
   deriving Eq

instance Show (Step l a) where
   show (Enter _) = "Enter"
   show (Exit _)  = "Exit"
   show (RuleStep _ r) = show r

instance Apply (Step l) where
   applyAll (RuleStep _ r) = applyAll r
   applyAll _              = return

instance Minor (Step l a) where
   setMinor b (RuleStep env r) = RuleStep env (setMinor b r)
   setMinor _ st = st

   isMinor (RuleStep _ r) = isMinor r
   isMinor _ = True

----------------------------------------------------------------------
-- State data type

data ParseState l a = PS
   { _value     :: a -- hidden
   , trace      :: [Step l a]
   , getPath    :: Path
   , _remainder :: Process (Step l a, a, Path) -- hidden
   }

makeState :: a -> Core l a -> ParseState l a
makeState a = PS a [] emptyPath . applyMin a . withPath . coreToProcess

majorOnly :: ParseState l a -> ParseState l a
majorOnly st = st { _remainder = hide p (_remainder st) }
 where
   p (x, _, _) = isMajor x

----------------------------------------------------------------------
-- Parse derivation tree

instance Firsts (ParseState l) where
   ready = ready . _remainder
   firsts pst = 
      [ (b, PS b (st:trace pst) path q) 
      | ((st, b, path), q) <- firsts (_remainder pst)
      ]
   
instance Minor (ParseState l a) where
   setMinor _ = id
   isMinor    = maybe False isMinor . listToMaybe . trace
        
searchModeState :: (Step l a -> Step l a -> Bool) -> ParseState l a -> ParseState l a
searchModeState eq state =
    state { _remainder = uniquePath eq' (_remainder (majorOnly state)) }
  where
    eq' = eq `on` fst3

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
applyMin a0 = prune (isMajor . fst3) . scan f a0
 where
   f a (RuleStep _ r, bs) =
      [ (b, (RuleStep env r, b, bs))
      | (b, env) <- transApply (transformation r) a
      ]
   f a (st, bs) = [(a, (st, a, bs))]

replay :: Monad m => Path -> Core l a -> a -> m (ParseState l a)
replay path core a = do
   (as, p) <- replayPath path $ withPath $ coreToProcess core
   return (PS a (map fst as) path (applyMin a p))