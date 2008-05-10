-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- A context for a term that maintains a current focus and an environment of
-- key-value pairs. A context is both showable and parsable.
--
-----------------------------------------------------------------------------
module Common.Context 
   ( -- * Abstract data type
     Context, inContext, fromContext, showContext, parseContext
     -- * Variable environment
   , Var(..), intVar, boolVar, get, set, change
     -- * Location (current focus)
   , Location, location, setLocation, changeLocation, currentFocus, changeFocus, locationDown, locationUp, makeLocation
     -- * Lifting rewrite rules
   , liftRuleToContext
     -- * Uniplate type class and utility functions
   , Uniplate(..), noUniplate, children, child, select, transform, transformAt, universe
   ) where

import Common.Utils
import Common.Transformation
import Control.Monad
import Data.Char
import Data.Dynamic
import Data.List
import Test.QuickCheck
import qualified Data.Map as M

----------------------------------------------------------
-- Abstract data type

-- | Abstract data type for a context: a context stores an envrionent (key-value pairs) and
-- a current focus (list of integers)
data Context a = C Location Environment a

instance Eq a => Eq (Context a) where
   x == y = fromContext x == fromContext y

instance Show a => Show (Context a) where
   show c = showContext c ++ ";" ++ show (fromContext c)

instance Functor Context where
   fmap f (C loc env a) = C loc env (f a)

instance Arbitrary a => Arbitrary (Context a) where
   arbitrary   = liftM inContext arbitrary
   coarbitrary = coarbitrary . fromContext

-- | Put a value into a (default) context
inContext :: a -> Context a
inContext = C (L []) M.empty

-- | Retrieve a value from its context
fromContext :: Context a -> a
fromContext (C _ _ a) = a

----------------------------------------------------------
-- A simple parser and pretty-printer for contexts

-- | Shows the context (without the embedded value)
showContext :: Context a -> String
showContext (C loc env _) = show loc ++ ";" ++ showEnv env

-- local helper function
showEnv :: Environment -> String
showEnv = concat . intersperse "," . map f . M.toList
 where f (k, (_, v)) = k ++ "=" ++ v

-- | Parses a context: on a successful parse, the unit value is returned in the parsed context
parseContext :: String -> Maybe (Context ())
parseContext s
   | all isSpace s = return (C (L []) M.empty ())
   | otherwise = do
        (loc, env)  <- splitAtElem ';' s
        if all isSpace env then return (C (read loc) M.empty ()) else do
        pairs       <- mapM (splitAtElem '=') (splitsWithElem ',' env)
        let f (k, v) = (k, (Nothing, v))
        return $ C (read loc) (M.fromList $ map f pairs) ()

----------------------------------------------------------
-- Manipulating the variable environment

-- local type synonym: can probably be simplified
type Environment = M.Map String (Maybe Dynamic, String)

-- | A variable has a name (for showing) and a default value (for initializing)
data Var a = String := a -- ^ Constructs a new variable

-- | Make a new variable of type Int (initialized with 0)
intVar :: String -> Var Int
intVar = (:= 0)

-- | Make a new variable of type Bool (initialized with True)
boolVar :: String -> Var Bool
boolVar = (:= True)

-- | Returns the value of a variable stored in a context
get :: (Read a, Typeable a) => Var a -> Context b -> a
get (s := a) (C loc env _) = 
   case M.lookup s env of
      Nothing           -> a           -- return default value
      Just (Just d,  s) -> fromDyn d a -- use the stored dynamic (default value as backup)
      Just (Nothing, s) -> 
         case reads s of               -- parse the pretty-printed value (default value as backup)
            [(b, rest)] | all isSpace rest -> b
            _ -> a

-- | Replaces the value of a variable stored in a context
set :: (Show a, Typeable a) => Var a -> a -> Context b -> Context b
set (s := _) a (C loc env b) = C loc (M.insert s (Just (toDyn a), show a) env) b

-- | Updates the value of a variable stored in a context
change :: (Show a, Read a, Typeable a) => Var a -> (a -> a) -> Context b -> Context b
change v f c = set v (f (get v c)) c
  
----------------------------------------------------------
-- Location (current focus)

-- | Type synonym for the current location (focus)
newtype Location = L [Int] deriving (Eq, Ord)

instance Show Location where
   show (L is) = show is

instance Read Location where
   readsPrec n s = [ (L is, rest) | (is, rest) <- readsPrec n s ]
   
-- | Returns the current location of a context
location :: Context a -> Location
location (C loc _ _) = loc

-- | Replaces the current location of a context
setLocation :: Location -> Context a -> Context a 
setLocation loc (C _ env a) = C loc env a

-- | Updates the current location of a context
changeLocation :: (Location -> Location) -> Context a -> Context a
changeLocation f c = setLocation (f (location c)) c

-- | Returns the term which has the current focus: Nothing indicates that the current 
-- focus is invalid
currentFocus :: Uniplate a => Context a -> Maybe a
currentFocus c = select (location c) (fromContext c)

-- | Changes the term which has the current focus. In case the focus is invalid, then
-- this function has no effect.
changeFocus :: Uniplate a => (a -> a) -> Context a -> Context a
changeFocus f c = fmap (transformAt (location c) f) c

-- | Go down to a certain child
locationDown :: Int -> Location -> Location
locationDown i (L is) = L (is ++ [i])

-- | Go up: Nothing indicates that we were already at the top
locationUp :: Location -> Maybe Location
locationUp (L is)
   | null is   = Nothing
   | otherwise = Just (L (init is))

makeLocation :: [Int] -> Location
makeLocation = L

----------------------------------------------------------
-- Lifting rewrite rules

-- | Lift a rule to operate on a term in a context
liftRuleToContext :: Uniplate a => Rule a -> Rule (Context a)
liftRuleToContext = lift $ makeLiftPair currentFocus (changeFocus . const)
   
---------------------------------------------------------
-- Uniplate class for generic traversals

-- | The Uniplate type class offers some light-weight functions for generic traversals. Only
-- a minimal set of operations are supported
class Uniplate a where
   uniplate :: a -> ([a], [a] -> a)    -- ^ Function for generic traversals

-- | Helper function to define a Uniplate instance for domains which don't support generic traversals
noUniplate :: a -> ([a], [a] -> a)
noUniplate a = ([], const a)

-- | Returns all the immediate children of a term
children :: Uniplate a => a -> [a]
children = fst . uniplate

-- | Selects one immediate child of a term. Nothing indicates that the child does not exist
child :: Uniplate a => Int -> a -> Maybe a
child n = safeHead . drop n . children 
               
-- | Selects a child based on a path. Nothing indicates that the path is invalid
select :: Uniplate a => Location -> a -> Maybe a
select (L is) a = foldM (flip child) a is

-- | Transforms one immediate child at a given index.
transform :: Uniplate a => Int -> (a -> a) -> a -> a
transform n f a = 
   let (as, build) = uniplate a 
       g i = if i==n then f else id
   in build (zipWith g [0..] as)

-- | Transforms one child based on a path.
transformAt :: Uniplate a => Location -> (a -> a) -> a -> a
transformAt (L is) f = foldr transform f is

-- | Returns all subterms
universe :: Uniplate a => a -> [a]
universe a = a : [ c | b <- children a, c <- universe b ]