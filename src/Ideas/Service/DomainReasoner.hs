{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Ideas.Service.DomainReasoner
   ( DomainReasoner(..)
   , exercisesSorted, servicesSorted
   , findExercise, findService
   , defaultScript -- , readScript
   ) where
   
import Ideas.Common.Utils
import Ideas.Common.Utils.TestSuite
import Ideas.Service.FeedbackScript.Parser
import Ideas.Service.Types
import Ideas.Common.Library
import Data.Ord
import Data.List
import Data.Maybe
import Data.Monoid

-----------------------------------------------------------------------
-- Domain Reasoner data type

data DomainReasoner = DR
   { reasonerId  :: Id
   , exercises   :: [Some Exercise]
   , services    :: [Service]
   , views       :: [ViewPackage]
   , aliases     :: [(Id, Id)]
   , scripts     :: [(Id, FilePath)]
   , testSuite   :: TestSuite
   , version     :: String
   , fullVersion :: String
   }

instance Monoid DomainReasoner where
   mempty = DR mempty mempty mempty mempty mempty mempty mempty mempty mempty 
   mappend c1 c2 = DR
      { reasonerId  = reasonerId c1  <> reasonerId c2
      , exercises   = exercises c1   <> exercises c2
      , services    = services c1    <> services c2
      , views       = views c1       <> views c2 
      , aliases     = aliases c1     <> aliases c2
      , scripts     = scripts c1     <> scripts c2 
      , testSuite   = testSuite c1   <> testSuite c2
      , version     = version c1     <> version c2
      , fullVersion = fullVersion c1 <> fullVersion c2
      }

instance HasId DomainReasoner where
   getId = reasonerId
   changeId f dr = dr { reasonerId = f (reasonerId dr) }

instance Typed a DomainReasoner where
   -- ignores views, testSuite
   typed = Tag "DomainReasoner" $ Iso (f <-> g) typed
    where
      f ((rid, ex, serv), (al, scr), (v, fv)) =
         DR rid ex serv [] al scr mempty v fv
      g dr = ( (reasonerId dr, exercises dr, services dr)
             , (aliases dr, scripts dr)
             , (version dr, fullVersion dr)
             )

-----------------------------------------------------------------------
-- Domain Reasoner functions

exercisesSorted :: DomainReasoner -> [Some Exercise]
exercisesSorted = sortBy (comparing f) . exercises
 where
   f :: Some Exercise -> String
   f (Some ex) = showId ex

servicesSorted :: DomainReasoner -> [Service]
servicesSorted = sortBy (comparing showId) . services

findExercise :: Monad m => DomainReasoner -> Id -> m (Some Exercise)
findExercise dr i =
   case [ a | a@(Some ex) <- exercises dr, getId ex == realName ] of
      [this] -> return this
      _      -> fail $ "Exercise " ++ show i ++ " not found"
 where
   realName = fromMaybe i (lookup i (aliases dr))

findService :: Monad m => DomainReasoner -> Id -> m Service
findService dr a 
   | null (qualifiers a) = -- search for unqualified string
        findWith (\s -> unqualified s == unqualified a) 
   | otherwise = 
        findWith (\s -> getId s == a)
 where
   findWith p  = single $ filter p $ services dr
 
   single []   = fail $ "No service " ++ showId a
   single [hd] = return hd
   single _    = fail $ "Ambiguous service " ++ showId a

defaultScript :: DomainReasoner -> Id -> IO Script
defaultScript dr = 
   maybe (return mempty) parseScriptSafe . (`lookup` scripts dr)