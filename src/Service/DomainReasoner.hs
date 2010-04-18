-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Service.DomainReasoner 
   ( DomainReasoner, DomainReasonerT
   , setVersion, addPackages, addServices, addPkgService
   , getPackages, getExercises, getServices, getVersion, lift, runDomainReasoner
   , liftIO, setFullVersion, toDomainReasoner, addPackage, addService
   ) where

import Common.Exercise
import Common.Utils (Some(..))
import Control.Monad.State
import Service.ServiceList
import Service.ExercisePackage

data Content = Content
   { packages :: [Some ExercisePackage]
   , services :: [Some ExercisePackage] -> [Some Service]
   , version  :: String
   }
   
emptyState :: Content
emptyState = Content [] (const []) []

type DomainReasonerT = StateT Content 
type DomainReasoner  = DomainReasonerT IO

runDomainReasoner :: Monad m => DomainReasonerT m a -> m a
runDomainReasoner = flip evalStateT emptyState

addPackages :: Monad m => [Some ExercisePackage] -> DomainReasonerT m ()
addPackages xs = modify $ \c -> c { packages = xs ++ packages c }

addPackage :: Monad m => Some ExercisePackage -> DomainReasonerT m ()
addPackage pkg = addPackages [pkg]

addServices :: Monad m => [Service a] -> DomainReasonerT m ()
addServices = mapM_ (addPkgService . const)

addService :: Monad m => Service a -> DomainReasonerT m ()
addService s = addServices [s]

addPkgService :: Monad m => ([Some ExercisePackage] -> Service a) -> DomainReasonerT m ()
addPkgService f = modify $ \c -> c { services = \xs -> Some (f xs) : services c xs }

setVersion :: Monad m => String -> DomainReasonerT m ()
setVersion s = modify $ \c -> c { version = s }

setFullVersion :: Monad m => String -> DomainReasonerT m ()
setFullVersion s = modify $ \c -> c  { version = s }

getPackages :: Monad m => DomainReasonerT m [Some ExercisePackage]
getPackages = gets packages

getExercises :: Monad m => DomainReasonerT m [Some Exercise]
getExercises = gets (map (\(Some pkg) -> Some (exercise pkg)) . packages)

getServices :: Monad m => DomainReasonerT m [Some Service]
getServices = gets (\c -> services c (packages c))

getVersion :: Monad m => DomainReasonerT m String
getVersion = gets version

toDomainReasoner :: Monad m => (m a -> IO a) -> DomainReasonerT m a -> DomainReasoner a
toDomainReasoner f m = get >>= lift . f . evalStateT m