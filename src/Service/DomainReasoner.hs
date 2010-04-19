{-# OPTIONS -XMultiParamTypeClasses -XTypeSynonymInstances #-}
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
   ( DomainReasoner
   , setVersion, addPackages, addServices, addPkgService
   , getPackages, getExercises, getServices, getVersion, runDomainReasoner
   , liftIO, setFullVersion, addPackage, addService
   , getFullVersion, runDomainReasoner2, liftEither, catchError
   , findPackage
   ) where

import Common.Exercise
import Common.Utils (Some(..))
import Control.Monad.Error
import Control.Monad.State
import Service.ServiceList
import Service.ExercisePackage

data Content = Content
   { packages    :: [Some ExercisePackage]
   , services    :: [Some ExercisePackage] -> [Some Service]
   , version     :: String
   , fullVersion :: Maybe String
   }
   
emptyState :: Content
emptyState = Content [] (const []) [] Nothing

newtype DomainReasoner a = DR { unDR :: ErrorT String (StateT Content IO) a }

instance Monad DomainReasoner where
   return a   = DR (return a)
   DR m >>= f = DR (m >>= unDR . f)
   fail s     = DR (fail s)

instance MonadError String DomainReasoner where
   throwError     = fail
   catchError m f = DR (unDR m `catchError` (unDR . f))

instance MonadState Content DomainReasoner where
   get   = DR get
   put s = DR (put s)

instance MonadIO DomainReasoner where
   liftIO m = DR (liftIO m)

runDomainReasoner :: DomainReasoner a -> IO a
runDomainReasoner m = runDomainReasoner2 m >>= either fail return

runDomainReasoner2 :: DomainReasoner a -> IO (Either String a)
runDomainReasoner2 m = do
   evalStateT (runErrorT (unDR m)) emptyState

liftEither :: Either String a -> DomainReasoner a
liftEither = either fail return

addPackages :: [Some ExercisePackage] -> DomainReasoner ()
addPackages xs = modify $ \c -> c { packages = xs ++ packages c }

addPackage :: Some ExercisePackage -> DomainReasoner ()
addPackage pkg = addPackages [pkg]

addServices :: [Service a] -> DomainReasoner ()
addServices = mapM_ (addPkgService . const)

addService :: Service a -> DomainReasoner ()
addService s = addServices [s]

addPkgService :: ([Some ExercisePackage] -> Service a) -> DomainReasoner ()
addPkgService f = modify $ \c -> c { services = \xs -> Some (f xs) : services c xs }

setVersion :: String -> DomainReasoner ()
setVersion s = modify $ \c -> c { version = s }

setFullVersion :: String -> DomainReasoner ()
setFullVersion s = modify $ \c -> c  { fullVersion = Just s }

getPackages :: DomainReasoner [Some ExercisePackage]
getPackages = gets packages

getExercises :: DomainReasoner [Some Exercise]
getExercises = gets (map (\(Some pkg) -> Some (exercise pkg)) . packages)

getServices :: DomainReasoner [Some Service]
getServices = gets (\c -> services c (packages c))

getVersion :: DomainReasoner String
getVersion = gets version

getFullVersion :: DomainReasoner String
getFullVersion = gets fullVersion >>= maybe getVersion return

findPackage :: ExerciseCode -> DomainReasoner (Some ExercisePackage)
findPackage code = do
   pkgs <- getPackages 
   let p (Some pkg) = exerciseCode (exercise pkg) == code
   case filter p pkgs of
      [this] -> return this
      _      -> fail $ "Package " ++ show code ++ " not found"