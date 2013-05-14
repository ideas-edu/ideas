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
   ( DomainReasoner(..), newDomainReasoner
   , findExercise, findService, defaultScript, readScript
   ) where
   
import Ideas.Common.Utils
import Ideas.Common.Utils.TestSuite
import Ideas.Service.FeedbackScript.Parser
import Ideas.Service.Types (Service)
import Ideas.Common.Library
import Data.Maybe
import Data.Monoid
import Control.Monad.Error

-----------------------------------------------------------------------
-- Domain Reasoner data type

data DomainReasoner = DR
   { exercises   :: [Some Exercise]
   , services    :: [Service]
   , views       :: [ViewPackage]
   , aliases     :: [(Id, Id)]
   , scriptDirs  :: [FilePath]
   , scripts     :: [(Id, FilePath)]
   , testSuite   :: TestSuite
   , version     :: String
   , fullVersion :: String
   }

instance Monoid DomainReasoner where
   mempty = DR mempty mempty mempty mempty mempty mempty mempty mempty mempty 
   mappend c1 c2 = DR
      { exercises   = exercises c1   <> exercises c2
      , services    = services c1    <> services c2
      , views       = views c1       <> views c2 
      , aliases     = aliases c1     <> aliases c2
      , scriptDirs  = scriptDirs c1  <> scriptDirs c2 
      , scripts     = scripts c1     <> scripts c2 
      , testSuite   = testSuite c1   <> testSuite c2
      , version     = version c1     <> version c2
      , fullVersion = fullVersion c1 <> fullVersion c2
      }

newDomainReasoner :: DomainReasoner      
newDomainReasoner = mempty

-----------------------------------------------------------------------
-- Domain Reasoner data type

findExercise :: Monad m => DomainReasoner -> Id -> m (Some Exercise)
findExercise dr i =
   case [ a | a@(Some ex) <- exercises dr, getId ex == realName ] of
      [this] -> return this
      _      -> fail $ "Exercise " ++ show i ++ " not found"
 where
   realName = fromMaybe i (lookup i (aliases dr))

findService :: Monad m => DomainReasoner -> String -> m Service
findService dr txt = do
   case filter ((==txt) . showId) (services dr) of
      [hd] -> return hd
      []   -> fail $ "No service " ++ txt
      _    -> fail $ "Ambiguous service " ++ txt

defaultScript :: DomainReasoner -> Id -> IO Script
defaultScript dr = 
   maybe (return mempty) (readScript dr) . (`lookup` scripts dr)

-- | Returns an empty script if the file does not exist
readScript :: DomainReasoner -> FilePath -> IO Script
readScript dr file = msum $
   [ parseScript (Just d) file | d <- scriptDirs dr ]
   ++ [ parseScript Nothing file
      , return mempty
      ]