{-# LANGUAGE GADTs #-}
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
module Common.Focus
   ( Focus(..), changeM, leave
   , MyNavigator
   , navigator, noNavigator, viewNavigator
   , currentT, castT
   ) where

import Common.Navigator
import Common.Utils.Uniplate
import Common.Rewriting.Term
import Common.View hiding (left, right)
import Control.Monad
import Data.List
import Data.Maybe
import Data.Generics.Str

class Focus f where
   current :: f a -> a
   change  :: (a -> a) -> f a -> f a
   replace :: a -> f a -> f a
   -- default definitions
   replace = change . const

changeM :: (Monad m, Focus f) => (a -> m a) -> f a -> m (f a)
changeM f a = liftM (`replace` a) (f (current a))

leave :: (Focus f, Navigator (f a)) => f a -> a
leave = current . top

---------------------------------------------------------------
-- Uniform navigator type

instance Show a => Show (MyNavigator a) where
   show a = show (leave a) ++ "   { "
            ++ show (current a)
            ++ " @ " ++ show (location a) ++ " }"

---------------------------------------------------------------
-- Constructors

navigator :: Uniplate a => a -> MyNavigator (Maybe a)
navigator = Simple . makeUniNav

noNavigator :: a -> MyNavigator (Maybe a)
noNavigator = NoNav . Just

viewNavigator :: IsTerm a => a -> MyNavigator (Maybe a)
viewNavigator = ViewNav . makeUniNav . Spine . toTerm

newtype SpineTerm = Spine {fromSpine :: Term}

instance Uniplate SpineTerm where
   uniplate a
      | null xs   = plate a
      | otherwise = plate spineTerm |* Spine x ||* map Spine xs
    where
      (x, xs) = getSpine (fromSpine a)
      spineTerm b = Spine . makeTerm (fromSpine b) . map fromSpine

data MyNavigator a where
   ViewNav :: IsTerm a => UniNav SpineTerm -> MyNavigator (Maybe a)
   Simple  :: (Navigator (f a), Focus f) => f a -> MyNavigator (Maybe a)
   NoNav   :: a -> MyNavigator a
   
instance Navigator (MyNavigator a) where
   up (ViewNav a) = liftM ViewNav (up a)
   up (Simple a)  = liftM Simple (up a)
   up (NoNav _)   = Nothing
   
   down (ViewNav a) = liftM ViewNav (down a)
   down (Simple a)  = liftM Simple (down a)
   down (NoNav _)   = Nothing
   
   left (ViewNav a) = liftM ViewNav (left a)
   left (Simple a)  = liftM Simple (left a)
   left (NoNav _)   = Nothing
   
   right (ViewNav a) = liftM ViewNav (right a)
   right (Simple a)  = liftM Simple (right a)
   right (NoNav _)   = Nothing
   
   downs (ViewNav a) = liftM ViewNav (downs a)
   downs (Simple a)  = map Simple (downs a)
   downs (NoNav _)   = []
   
   location (ViewNav a) = location a
   location (Simple a)  = location a
   location (NoNav _)   = []
   
instance Focus MyNavigator where
   current (ViewNav a) = matchM termView (fromSpine (current a))
   current (Simple a)  = Just (current a)
   current (NoNav a)   = a
   
   change f (ViewNav a) = 
      let g = Spine . simplifyWithM (f . Just) termView . fromSpine
      in ViewNav (change g a)
   change f (Simple a) = Simple (change (\x -> fromMaybe x (f (Just x))) a)
   change f (NoNav a)  = NoNav (f a)

currentT :: MyNavigator a -> Maybe Term
currentT (ViewNav a) = Just (fromSpine (current a))
currentT _           = Nothing
   
castT :: IsTerm b => MyNavigator (Maybe a) -> MyNavigator (Maybe b)
castT (ViewNav a) = ViewNav a
castT _           = NoNav Nothing

---------------------------------------------------------------

data ListIterator a = LI [a] a [a]
   
instance Show a => Show (ListIterator a) where
   show (LI xs y ys) = 
      let listLike   = brackets . intercalate ","
          brackets s = "[" ++ s ++ "]"
          focusOn  s = "<<" ++ s ++ ">>"
      in listLike (map show (reverse xs) ++ focusOn (show y) : map show ys)

instance Focus ListIterator where
   current  (LI _ x _)   = x
   change f (LI xs y ys) = LI xs (f y) ys
   
instance Iterator (ListIterator a) where
   previous (LI (x:xs) y ys) = Just (LI xs x (y:ys))
   previous _                = Nothing
   
   next (LI xs x (y:ys)) = Just (LI (x:xs) y ys)
   next _                = Nothing

fromListIterator :: ListIterator a -> [a]
fromListIterator (LI xs y ys) = reverse xs ++ y : ys

toListIterator :: [a] -> Maybe (ListIterator a)
toListIterator []     = Nothing
toListIterator (x:xs) = Just (LI [] x xs)

posListIterator :: ListIterator a -> Int
posListIterator (LI xs _ _) = length xs

---------------------------------------------------------------

data StrIterator a = SI a !Int [Either (Str a) (Str a)]

instance Show a => Show (StrIterator a) where
   show a = show (current a) ++ " @ " ++ show (posStrIterator a)

instance Focus StrIterator where
   current  (SI a _ _)  = a
   change f (SI a n xs) = SI (f a) n xs
   
instance Iterator (StrIterator a) where
   previous (SI a n xs) = rec xs (One a)
    where
      rec []     _ = Nothing
      rec (y:ys) s = 
         let make b = case lastStrIterator b of
                         Just (SI c _ zs) -> Just (SI c (n-1) (zs++Left s:ys))
                         Nothing -> rec ys (b `Two` s)
         in either (rec ys . (s `Two`)) make y 

   next (SI a n xs) = rec xs (One a)
    where
      rec [] _ = Nothing
      rec (y:ys) s = 
         let make b = case firstStrIterator b of
                         Just (SI c _ zs) -> Just (SI c (n+1) (zs++Right s:ys))
                         Nothing -> rec ys (s `Two` b)
         in either make (rec ys . (`Two` s)) y

fromStrIterator :: StrIterator a -> Str a
fromStrIterator (SI a _ xs) = rec xs (One a)
 where
   rec []     s = s
   rec (y:ys) s = rec ys (either (s `Two`) (`Two` s) y)

singleStrIterator :: a -> StrIterator a
singleStrIterator a = SI a 0 []

firstStrIterator :: Str a -> Maybe (StrIterator a)
firstStrIterator = rec []
 where
   rec acc str =
      case str of
         Zero    -> Nothing
         One a   -> Just (SI a 0 acc)
         Two a b -> rec (Left b:acc) a  
                `mplus`
                    rec (Right a:acc) b

lastStrIterator :: Str a -> Maybe (StrIterator a)
lastStrIterator = rec 0 []
 where
   rec n acc str = 
      case str of
         Zero    -> Nothing
         One a   -> Just (SI a n acc)
         Two a b -> 
            rec (n+countStr a) (Right a:acc) b 
          `mplus` 
            rec n (Left b:acc) a

posStrIterator :: StrIterator a -> Int
posStrIterator (SI _ n _) = n

countStr :: Str a -> Int
countStr Zero      = 0
countStr (One _)   = 1
countStr (Two a b) = countStr a + countStr b

s = firstStrIterator $ Two (Two (One 1) (One 2)) (Two (Two (One 3) (One 4)) (One 5))

---------------------------------------------------------------

data UniNav a = U (StrIterator a) [StrIterator a -> StrIterator a] 

instance (Show a, Uniplate a ) => Show (UniNav a) where
   show a = show (current a) ++ " @ " ++ show (location a)

instance Uniplate a => Navigator (UniNav a) where
   up (U _ [])     = Nothing
   up (U a (f:fs)) = Just (U (f a) fs)
  
   down (U a fs) = do
      let (cs, g) = uniplate (current a)
          f x = replace (g (fromStrIterator x)) a
      b <- firstStrIterator cs
      return (U b (f:fs))
      
   left  (U a fs) = liftM (\b -> U b fs) (previous a)
   right (U a fs) = liftM (\b -> U b fs) (next a)
   
   downs a = maybe [] rec (down a)
    where
      rec x = x : maybe [] rec (right x)
      
   location (U a fs) = reverse (rec a fs)
    where
      rec _ [] = []
      rec b (g:gs) = posStrIterator b : rec (g b) gs

instance Focus UniNav where
   current  (U a _)  = current a
   change f (U a xs) = U (change f a) xs

data T = T Int [T] deriving Show

instance Uniplate T where
   uniplate (T a xs) = plate (T a) ||* xs

makeUniNav :: a -> UniNav a
makeUniNav a = U (singleStrIterator a) []
   
ex = U (singleStrIterator $
   T 0 [T 1 [], T 2 [T 3 [], T 4 [], T 5 []], T 6 []]) []
   
{-
data StrIterator a 
   = Here a 
   | TwoLeft  (StrIterator a) (Str a) 
   | TwoRight (Str a) (StrIterator a)

instance Show a => Show (StrIterator a) where
   show a = show (current a) ++ " @ " ++ show (posStrIterator a)

instance Focus StrIterator where
   current (Here a)       = a
   current (TwoLeft a _)  = current a
   current (TwoRight _ a) = current a
   change f (Here a)       = Here (f a)
   change f (TwoLeft a b)  = TwoLeft (change f a) b
   change f (TwoRight a b) = TwoRight a (change f b)

instance Iterator (StrIterator a) where
   previous (Here _) = Nothing
   previous (TwoLeft a b) = liftM (`TwoLeft` b) (previous a)
   previous (TwoRight a b) = liftM (TwoRight a) (previous b) `mplus`
                             liftM (`TwoLeft` fromStrIterator b) (lastStr a)
   next (Here _) = Nothing
   next (TwoLeft a b) = liftM (`TwoLeft` b) (next a) `mplus`
                        liftM (TwoRight (fromStrIterator a)) (toStrIterator b)
   next (TwoRight a b) = liftM (TwoRight a) (next b)

fromStrIterator :: StrIterator a -> Str a
fromStrIterator (Here a)       = One a
fromStrIterator (TwoLeft a b)  = Two (fromStrIterator a) b
fromStrIterator (TwoRight a b) = Two a (fromStrIterator b)

toStrIterator :: Str a -> Maybe (StrIterator a)
toStrIterator Zero      = Nothing
toStrIterator (One a)   = Just (Here a)
toStrIterator (Two a b) = 
   liftM (`TwoLeft` b) (toStrIterator a) `mplus`
   liftM (TwoRight a) (toStrIterator b)

lastStr :: Str a -> Maybe (StrIterator a)
lastStr Zero = Nothing
lastStr (One a) = Just (Here a)
lastStr (Two a b) = 
   liftM (TwoRight a) (lastStr b) `mplus`
   liftM (`TwoLeft` b) (lastStr a)

posStrIterator :: StrIterator a -> Int
posStrIterator (Here _)       = 0
posStrIterator (TwoLeft a _)  = posStrIterator a
posStrIterator (TwoRight a b) = posStrIterator b + countStr a

countStr :: Str a -> Int
countStr Zero      = 0
countStr (One _)   = 1
countStr (Two a b) = countStr a + countStr b
-}