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
   ( Context , inContext, fromContext, showContext, parseContext
     -- Variable environment
   , Var(..), intVar, boolVar, get, set, change
     -- Location (current focus)
   , Location, location, setLocation, changeLocation, currentFocus, changeFocus
     -- Lifting rewrite rules
   , liftRuleToContext
     -- Uniplate type class and utility functions
   , Uniplate(..), noUniplate, children, child, select, transform, transformAt
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
-- Context abstract data type and core operations

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

inContext :: a -> Context a
inContext = C [] M.empty

fromContext :: Context a -> a
fromContext (C _ _ a) = a

----------------------------------------------------------
-- A simple parser and pretty-printer for contexts

showContext :: Context a -> String
showContext (C loc env _) = show loc ++ ";" ++ showEnv env

-- local helper function
showEnv :: Environment -> String
showEnv = concat . intersperse "," . map f . M.toList
 where f (k, (_, v)) = k ++ "=" ++ v
 
parseContext :: String -> Maybe (Context ())
parseContext s = do
   (loc, env)  <- splitAtElem ';' s
   pairs       <- mapM (splitAtElem '=') (splitsWithElem ',' env)
   let f (k, v) = (k, (Nothing, v))
   return $ C (read loc) (M.fromList $ map f pairs) ()

----------------------------------------------------------
-- Manipulating the variable environment

-- local type synonym: can probably be simplified
type Environment = M.Map String (Maybe Dynamic, String)

-- A variable has a name (for showing) and a default value (for initializing)
data Var a = String := a

intVar :: String -> Var Int
intVar = (:= 0)

boolVar :: String -> Var Bool
boolVar = (:= True)

get :: (Read a, Typeable a) => Var a -> Context b -> a
get (s := a) (C loc env _) = 
   case M.lookup s env of
      Nothing           -> a           -- return default value
      Just (Just d,  s) -> fromDyn d a -- use the stored dynamic (default value as backup)
      Just (Nothing, s) -> 
         case reads s of               -- parse the pretty-printed value (default value as backup)
            [(b, rest)] | all isSpace rest -> b
            _ -> a

set :: (Show a, Typeable a) => Var a -> a -> Context b -> Context b
set (s := _) a (C loc env b) = C loc (M.insert s (Just (toDyn a), show a) env) b

change :: (Show a, Read a, Typeable a) => Var a -> (a -> a) -> Context b -> Context b
change v f c = set v (f (get v c)) c
  
----------------------------------------------------------
-- Location (current focus)

type Location = [Int]

location :: Context a -> Location
location (C loc _ _) = loc

setLocation :: Location -> Context a -> Context a 
setLocation loc (C _ env a) = C loc env a

changeLocation :: (Location -> Location) -> Context a -> Context a
changeLocation f c = setLocation (f (location c)) c

currentFocus :: Uniplate a => Context a -> Maybe a
currentFocus c = select (location c) (fromContext c)

changeFocus :: Uniplate a => (a -> a) -> Context a -> Context a
changeFocus f c = fmap (transformAt (location c) f) c

----------------------------------------------------------
-- Lifting rewrite rules

liftRuleToContext :: Uniplate a => Rule a -> Rule (Context a)
liftRuleToContext = lift $ makeLiftPair currentFocus (changeFocus . const)
   
---------------------------------------------------------
-- Uniplate class for generic traversals

class Uniplate a where
   uniplate :: a -> ([a], [a] -> a)

noUniplate :: a -> ([a], [a] -> a)
noUniplate a = ([], const a)

children :: Uniplate a => a -> [a]
children = fst . uniplate

child :: Uniplate a => Int -> a -> Maybe a
child n = safeHead . drop n . children 
               
select :: Uniplate a => [Int] -> a -> Maybe a
select = flip $ foldM $ flip child

transform :: Uniplate a => Int -> (a -> a) -> a -> a
transform n f a = 
   let (as, build) = uniplate a 
       g i = if i==n then f else id
   in build (zipWith g [0..] as)

transformAt :: Uniplate a => [Int] -> (a -> a) -> a -> a
transformAt = flip (foldr transform)