{-# LANGUAGE TypeFamilies #-}
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
-- The path can be used to reconstruct the sequence of steps already performed 
-- (a so-called trace).
--
-----------------------------------------------------------------------------
--  $Id$

module Ideas.Common.Strategy.Parsing
   ( -- * Running @Core@ strategies
     runCore
     -- * Prefix
   , Prefix, makePrefix, replayCore, replayAndApply
   , majorPrefix, searchModePrefix, prefixPath
     -- * Step
   , Step(..)
     -- * Path
   , Path, emptyPath
   ) where

import Data.Function
import Data.Maybe
import Ideas.Common.Classes
import Ideas.Common.Environment
import Ideas.Common.Id
import Ideas.Common.Rule
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Core
import Ideas.Common.Strategy.Derived
import Ideas.Common.Strategy.Process
import Ideas.Common.Strategy.Sequence
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
         a :>|> b   -> rec a >|> rec b
         a :|>: b   -> rec a |> rec b
         Rule r     -> single (Single (RuleStep mempty r))
         Fail       -> empty
         Succeed    -> done
         Label l a  
            | useLabels -> Single (Enter l) ~> rec a
                           <*> single (Single (Exit l))
            | otherwise -> rec a
         a :%: b    -> concurrent switch (rec a) (rec b)
         a :@: b    -> rec a <@> rec b
         Atomic a   -> atomic (rec a)
         Not a      -> notCore a
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

notCore :: Core a -> Builder (Sym (Step a))
notCore core = single $ Single $ RuleStep mempty $ 
   checkRule "core.not" $ null . runCore core

--------------------------------------------------------------------------------
-- Prefix datatype

data Prefix a = Prefix
   { getPath   :: Path
   , remainder :: Process (Step a, a, Path)
   }

instance Show (Prefix a) where
   show = show . prefixPath

instance Firsts (Prefix a) where
   type Elem (Prefix a) = (Step a, a)

   menu = fmap f . menu . remainder
    where
      f Done = Done
      f ((st, a, path) :~> p) = (st, a) :~> Prefix path p 

--------------------------------------------------------------------------------
-- Constructing a prefix

-- | Make a prefix from a core strategy and a start term.
makePrefix :: Core a -> a -> Prefix a
makePrefix core = snd . fromJust . replayCore emptyPath core
 
-- | Construct a prefix by replaying a path in a core strategy: the third 
-- argument is the current term.
replayCore :: Path -> Core a -> a -> Maybe ([Step a], Prefix a)
replayCore path core a = do
   let p = withPath (coreToProcess True core)
   (acc, q) <- runPath path p
   let steps = map fst acc
       prfx = Prefix path (applySteps q a)
   return (steps, prfx)

replayAndApply :: Path -> Core a -> a -> Maybe (a, Prefix a)
replayAndApply path core a = do
   let p = withPath (coreToProcess True core)
   (acc, q) <- runPath path p
   b <- applyList (map fst acc) a
   return $ (b, Prefix path (applySteps q b))
   
runPath :: Path -> Process a -> Maybe ([a], Process a)
runPath = rec [] . ints
 where
   rec acc []     p = Just (reverse acc, p)
   rec acc (n:ns) p =
      case getByIndex n (menu p) of
         Just (a :~> r) -> rec (a:acc) ns r
         _ -> Nothing
   
applySteps :: Process (Step a, Path) -> a -> Process (Step a, a, Path)
applySteps p a0 = prune (isMajor . fst3) (scan f a0 p)
 where
   f a (RuleStep _ r, path) =
      [ (b, (RuleStep env r, b, path))
      | (b, env) <- transApply (transformation r) a
      ]
   f a (st, path) = [(a, (st, a, path))]

withPath :: Process a -> Process (a, Path)
withPath = rec []
 where
   rec ns = mapWithIndex (menuItem done . f ns) . menu

   f ns n a p = 
      let ms = n:ns
      in (a, Path (reverse ms)) ~> rec ms p

--------------------------------------------------------------------------------
-- Prefix fuctions
      
-- | Transforms the prefix such that only major steps are kept in the remaining
-- strategy. 
majorPrefix :: Prefix a -> Prefix a
majorPrefix prfx = prfx { remainder = hide (isMajor . fst3) (remainder prfx) }

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