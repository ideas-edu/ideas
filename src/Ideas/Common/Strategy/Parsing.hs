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
-- Basic machinery for fully executing a core strategy expression, or only
-- partially. Partial execution results in a prefix that keeps the current.
-- location in the strategy (a @Path@) for continuing the execution later on.
-- A prefix also maintains the sequence of steps already performed (a so-called
-- trace).
--
-----------------------------------------------------------------------------
--  $Id$

module Ideas.Common.Strategy.Parsing
   ( -- * Running @Core@ strategies
     runCore
     -- * Prefix
   , Prefix, replayCore, majorPrefix, searchModePrefix
   , prefixPath, prefixToSteps, lastStepInPrefix, activeLabels
     -- * Step
   , Step(..)
     -- * Path
   , Path, emptyPath
   ) where

import Data.Function
import Data.List
import Data.Maybe
import Ideas.Common.Classes
import Ideas.Common.Environment
import Ideas.Common.Id
import Ideas.Common.Rule
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Core
import Ideas.Common.Strategy.Derived
import Ideas.Common.Strategy.Process
import Ideas.Common.Strategy.Sequence hiding (Step)
import Ideas.Common.Utils (fst3)
import Ideas.Common.Utils.Uniplate

--------------------------------------------------------------------------------
-- Running Core strategies

-- | Run a @Core@ strategy and return all results.
runCore :: Core a -> a -> [a]
runCore core a = bests $ accum applyAll a $ coreToProcess False core

coreToProcess :: Bool -> Core a -> Process (Step a)
coreToProcess useLabels = fromAtoms . toProcess . rec . coreSubstAll
 where
   rec :: Core a -> Builder (Sym (Step a))
   rec core =
      case core of
         a :*: b    -> rec a <*> rec b
         a :|: b    -> rec a <|> rec b
         Rule r     -> single (Single (RuleStep mempty r))
         a :|>: b   -> rec a |> rec b
         Fail       -> empty
         Succeed    -> done
         Label l a  
            | useLabels -> Single (Enter l) ~> rec a
                           <*> single (Single (Exit l))
            | otherwise -> rec a
         a :%: b    -> concurrent switch (rec a) (rec b)
         a :@: b    -> rec a <@> rec b
         Atomic a   -> atomic (rec a)
         Remove _   -> empty
         Collapse a -> rec (collapse a)
         Hide a     -> rec (fmap minor a)
         Let _ _    -> error "not substituted: let"
         Var _      -> error "not substituted: var"

   switch (Single (Enter _)) = False
   switch _ = True

collapse :: Core a -> Core a
collapse (Label l s) = Rule $ makeRule l (runCore s)
collapse core = descend collapse core

--------------------------------------------------------------------------------
-- Prefix datatype

data Prefix a = Prefix
   { _value    :: a
   , trace     :: [Step a]
   , getPath   :: Path
   , remainder :: Process (Step a, a, Path)
   }

instance Show (Prefix a) where
   show = show . prefixPath

instance Firsts Prefix where
   ready = ready . remainder
   firsts prfx = 
      [ (a, Prefix a (st:trace prfx) path q) 
      | ((st, a, path), q) <- firsts (remainder prfx)
      ]

instance Minor (Prefix a) where
   setMinor _ = id
   isMinor    = maybe False isMinor . lastStepInPrefix

--------------------------------------------------------------------------------
-- Constructing a prefix

-- | A prefix is constructed by replaying a path in a core strategy.
replayCore :: Path -> Core a -> a -> Prefix a
replayCore path core a0 =
   rec [] (ints path) $ withPath $ coreToProcess True core
 where 
   rec acc []     p = Prefix a0 (map fst acc) path (applySteps a0 p)
   rec acc (n:ns) p =
      case getByIndex n (menu p) of
         Just (a :~> r) -> rec (a:acc) ns r
         _ -> rec acc [] empty -- invalid path
   
applySteps :: a -> Process (Step a, Path) -> Process (Step a, a, Path)
applySteps a0 = prune (isMajor . fst3) . scan f a0
 where
   f a (RuleStep _ r, path) =
      [ (b, (RuleStep env r, b, path))
      | (b, env) <- transApply (transformation r) a
      ]
   f a (st, path) = [(a, (st, a, path))]

withPath :: Process a -> Process (a, Path)
withPath = rec []
 where
   rec ns = mapWithIndex (step done . f ns) . menu

   f ns n a p = 
      let ms = n:ns
      in (a, Path (reverse ms)) ~> rec ms p

--------------------------------------------------------------------------------
-- Prefix fuctions
      
-- | Transforms the prefix such that only major steps are kept. 
majorPrefix :: Prefix a -> Prefix a
majorPrefix prfx = prfx 
   { remainder = hide (isMajor . fst3) (remainder prfx) 
   , trace     = filter isMajor (trace prfx)
   }

-- | The searchModePrefix transformation changes the process in such a way that 
--   all intermediate states can only be reached by one path. A prerequisite is 
--   that symbols are unique (or only used once).
searchModePrefix :: (Step a -> Step a -> Bool) -> Prefix a -> Prefix a
searchModePrefix eq prfx =
   prfx { remainder = rec (remainder (majorPrefix prfx)) }
 where
   eq3 = eq `on` fst3

   rec p | ready p   = done
         | otherwise = process (firsts p)
 
   process [] = empty
   process ((a, p):xs) = 
      let ys = map fst $ firsts (a ~> p)
      in (a ~> rec p) <|> process (concatMap (change ys) xs)

   change ys (a, q) = 
      let f x = all (not . eq3 x) ys
      in firsts $ filterP f (a ~> q)

-- | Returns the current @Path@.
prefixPath :: Prefix a -> Path
prefixPath = getPath

-- | Returns all steps that are applied so far.
prefixToSteps :: Prefix a -> [Step a]
prefixToSteps = reverse . trace

-- | Returns the last step of a prefix (if such a step exists)
lastStepInPrefix :: Prefix a -> Maybe (Step a)
lastStepInPrefix = listToMaybe . trace

-- | Calculate the active labels
activeLabels :: Prefix a -> [Id]
activeLabels prfx = nub [l | Enter l <- steps] \\ [l | Exit l <- steps]
 where
   steps = prefixToSteps prfx

--------------------------------------------------------------------------------
-- Step

-- | The steps during the parsing process: enter (or exit) a labeled 
-- sub-strategy, or a rule.
data Step a = Enter Id                      -- ^ Enter a labeled sub-strategy
            | Exit Id                       -- ^ Exit a labeled sub-strategy
            | RuleStep Environment (Rule a) -- ^ Rule that was applied
   deriving Eq

instance Show (Step a) where
   show (Enter _) = "Enter"
   show (Exit _)  = "Exit"
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

--------------------------------------------------------------------------------
-- Path

-- | A path encodes a location in a strategy. Paths are represented as a list
-- of integers. Use the @Show@ and @Read@ type classes for serializing.
newtype Path = Path { ints :: [Int] }
   deriving Eq

instance Show Path where
   show = show . ints

instance Read Path where
   readsPrec _ = map (mapFirst Path) . readList

-- | The empty path.
emptyPath :: Path
emptyPath = Path []