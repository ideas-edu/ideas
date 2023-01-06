-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Ideas.Service.DomainReasoner
   ( DomainReasoner(..), tDomainReasoner, newDomainReasoner
   , exercisesSorted, servicesSorted
   , findExercise, findService
   , defaultScript
   ) where

import Data.List
import Data.Maybe
import Data.Semigroup as Sem
import Ideas.Common.Library
import Ideas.Service.FeedbackScript.Parser
import Ideas.Service.Types
import Ideas.Utils.TestSuite

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

instance Sem.Semigroup DomainReasoner where
   c1 <> c2 = DR
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

instance Monoid DomainReasoner where
   mempty  = DR mempty mempty mempty mempty mempty mempty mempty mempty mempty
   mappend = (<>)

instance HasId DomainReasoner where
   getId = reasonerId
   changeId f dr = dr { reasonerId = f (reasonerId dr) }

tDomainReasoner :: Type a DomainReasoner
tDomainReasoner = Tag "DomainReasoner" $ Iso (f <-> g) tp
    where
      tp = tTuple3 (tTuple3 tId (tList tSomeExercise) (tList tService))
           (tPair (tList (tPair tId tId)) (tList (tPair tId tString)))
           (tPair tString tString)
      f ((rid, ex, serv), (al, scr), (v, fv)) =
         DR rid ex serv [] al scr mempty v fv
      g dr = ( (reasonerId dr, exercises dr, services dr)
             , (aliases dr, scripts dr)
             , (version dr, fullVersion dr)
             )

newDomainReasoner :: IsId a => a -> DomainReasoner
newDomainReasoner a = mempty {reasonerId  = newId a}

-----------------------------------------------------------------------
-- Domain Reasoner functions

exercisesSorted :: DomainReasoner -> [Some Exercise]
exercisesSorted = sortOn f . exercises
 where
   f :: Some Exercise -> String
   f (Some ex) = showId ex

servicesSorted :: DomainReasoner -> [Service]
servicesSorted = sortOn showId . services

findExercise :: DomainReasoner -> Id -> Either String (Some Exercise)
findExercise dr i =
   case [ a | a@(Some ex) <- exercises dr, getId ex == realName ] of
      []     -> Left $ "Exercise " ++ show i ++ " not found"
      [this] -> Right this
      _      -> Left $ "Ambiguous exercise " ++ show i
 where
   realName = fromMaybe i (lookup i (aliases dr))

findService :: DomainReasoner -> Id -> Either String Service
findService dr a
   | null (qualifiers a) = -- search for unqualified string
        findWith (\s -> unqualified s == unqualified a)
   | otherwise =
        findWith (\s -> getId s == a)
 where
   findWith p  = single $ filter p $ services dr

   single []   = Left $ "No service " ++ showId a
   single [hd] = Right hd
   single _    = Left $ "Ambiguous service " ++ showId a

defaultScript :: DomainReasoner -> Id -> IO Script
defaultScript dr n =
   maybe (return mempty) parseScriptSafe (realName `lookup` scripts dr)
 where
   realName = fromMaybe n (lookup n (aliases dr))