{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
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
   ( -- * Domain Reasoner data type
     DomainReasoner, runDomainReasoner, runWithCurrent
   , liftEither, MonadIO(..), catchError 
     -- * Update functions
   , addPackages, addPackage, addPkgService
   , addServices, addService, addTestSuite
   , setVersion, setFullVersion
     -- * Accessor functions
   , getPackages, getExercises, getServices
   , getVersion, getFullVersion, getTestSuite
   , findPackage, findService
   , defaultScript
   ) where

import Common.Library
import Common.TestSuite
import Common.Utils (Some(..))
import Control.Monad.Error
import Control.Monad.State
import Data.List
import Data.Maybe
import Service.Types
import Service.ExercisePackage
import Service.FeedbackScript
import Service.ScriptParser

-----------------------------------------------------------------------
-- Domain Reasoner data type

newtype DomainReasoner a = DR { unDR :: ErrorT String (StateT Content IO) a }

data Content = Content
   { packages    :: [Some ExercisePackage]
   , services    :: [Some ExercisePackage] -> [Service]
   , testSuite   :: TestSuite
   , version     :: String
   , fullVersion :: Maybe String
   }
   
noContent :: Content
noContent = Content [] (const []) (return ()) [] Nothing

runDomainReasoner :: DomainReasoner a -> IO a
runDomainReasoner m = do
   result <- evalStateT (runErrorT (unDR m)) noContent
   case result of
      Left msg -> fail msg
      Right a  -> return a

-- | Returns a run function, based on the current state, inside the monad
runWithCurrent :: DomainReasoner (DomainReasoner a -> IO a)
runWithCurrent =
   get >>= \st -> return (runDomainReasoner . (put st >>))

liftEither :: Either String a -> DomainReasoner a
liftEither = either throwError return

-----------------------------------------------------------------------
-- Instance declarations

instance Monad DomainReasoner where
   return a   = DR (return a)
   DR m >>= f = DR (m >>= unDR . f)
   fail s     = DR (throwError s)

instance MonadError String DomainReasoner where
   throwError s   = DR (throwError s)
   catchError m f = DR (unDR m `catchError` (unDR . f))

instance MonadPlus DomainReasoner where
   mzero       = DR mzero
   a `mplus` b = DR (unDR a `mplus` unDR b)

instance MonadState Content DomainReasoner where
   get   = DR get
   put s = DR (put s)

instance MonadIO DomainReasoner where
   liftIO m = DR (liftIO m)

-----------------------------------------------------------------------
-- Update functions

addPackages :: [Some ExercisePackage] -> DomainReasoner ()
addPackages xs = modify $ \c -> c { packages = xs ++ packages c }

addPackage :: Some ExercisePackage -> DomainReasoner ()
addPackage pkg = addPackages [pkg]

addPkgService :: ([Some ExercisePackage] -> Service) -> DomainReasoner ()
addPkgService f = modify $ \c -> 
   c { services = \xs -> f xs : services c xs }

addServices :: [Service] -> DomainReasoner ()
addServices = mapM_ (addPkgService . const)

addService :: Service -> DomainReasoner ()
addService s = addServices [s]

addTestSuite :: TestSuite -> DomainReasoner ()
addTestSuite m = modify $ \c -> c { testSuite = testSuite c >> m }

setVersion :: String -> DomainReasoner ()
setVersion s = modify $ \c -> c { version = s }

setFullVersion :: String -> DomainReasoner ()
setFullVersion s = modify $ \c -> c  { fullVersion = Just s }

-----------------------------------------------------------------------
-- Accessor functions

getPackages :: DomainReasoner [Some ExercisePackage]
getPackages = gets packages

getExercises :: DomainReasoner [Some Exercise]
getExercises = gets (map (\(Some pkg) -> Some (exercise pkg)) . packages)

getServices :: DomainReasoner [Service]
getServices = gets (\c -> services c (packages c))

getVersion :: DomainReasoner String
getVersion = gets version

getFullVersion :: DomainReasoner String
getFullVersion = gets fullVersion >>= maybe getVersion return

getTestSuite :: DomainReasoner TestSuite
getTestSuite = gets testSuite

findPackage :: Id -> DomainReasoner (Some ExercisePackage)
findPackage i = do
   pkgs <- getPackages 
   case [ a | a@(Some pkg) <- pkgs, getId pkg == resolveId i ] of
      [this] -> return this
      _      -> throwError $ "Package " ++ show i ++ " not found"
      
findService :: String -> DomainReasoner Service
findService txt = do
   srvs <- getServices
   case filter ((==txt) . showId) srvs of
      [hd] -> return hd
      []   -> throwError $ "No service " ++ txt
      _    -> throwError $ "Ambiguous service " ++ txt
      
defaultScript :: Id -> DomainReasoner Script
defaultScript a
   | ["logic", "propositional"] `isPrefixOf` qualifiers a = do
        liftIO $ parseScript "scripts/logic.txt" 
   | otherwise = throwError $ "No feedback script available for " ++ show a

-----------------------------------------------------------------------
-- Identifier aliases (temporary)

resolveId :: Id -> Id
resolveId i = fromMaybe i (lookup i table)
 where
   table = map (newId *** newId)
      [ ("math.coverup",             "algebra.equations.coverup")
      , ("math.lineq",               "algebra.equations.linear")
      , ("math.lineq-mixed",         "algebra.equations.linear.mixed")
      , ("math.quadreq",             "algebra.equations.quadratic")         
      , ("math.quadreq-no-abc",      "algebra.equations.quadratic.no-abc")    
      , ("math.quadreq-with-approx", "algebra.equations.quadratic.approximate")
      , ("math.higherdegree",        "algebra.equations.polynomial")
      , ("math.rationaleq",          "algebra.equations.rational")
      , ("math.linineq",             "algebra.inequalities.linear")
      , ("math.quadrineq",           "algebra.inequalities.quadratic")
      , ("math.ineqhigherdegree",    "algebra.inequalities.polynomial")
      , ("math.factor",              "algebra.manipulation.polynomial.factor")
      , ("math.simplifyrational",    "algebra.manipulation.rational.simplify")
      , ("math.simplifypower",       "algebra.manipulation.exponents.simplify")
      , ("math.nonnegexp",           "algebra.manipulation.exponents.nonnegative")
      , ("math.powerof",             "algebra.manipulation.exponents.powerof")
      , ("math.derivative",          "calculus.differentiation")
      , ("math.fraction",            "arithmetic.fractions")
      , ("math.calcpower",           "arithmetic.exponents")
      , ("linalg.gaussianelim",      "linearalgebra.gaussianelim")
      , ("linalg.gramschmidt",       "linearalgebra.gramschmidt")
      , ("linalg.linsystem",         "linearalgebra.linsystem")
      , ("linalg.systemwithmatrix",  "linearalgebra.systemwithmatrix")
      , ("logic.dnf",                "logic.propositional.dnf")
      , ("logic.dnf-unicode",        "logic.propositional.dnf.unicode")
      , ("relationalg.cnf",          "relationalgebra.cnf")
      -- MathDox compatibility
      , ("gaussianelimination"        , "linearalgebra.gaussianelim")
      , ("gramschmidt"                , "linearalgebra.gramschmidt")
      , ("solvelinearsystem"          , "linearalgebra.linsystem")
      , ("solvelinearsystemwithmatrix", "linearalgebra.systemwithmatrix")
      ]