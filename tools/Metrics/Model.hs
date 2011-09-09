module Model 
   ( System(..), systemModules
   , Module(..), Function(..)
   , Size, sizeOf, sizeLines, sizeChars, Sized(..), showSize
   , overallLines, overallChars, linesOfCode, charsOfCode
   , Named(..), QName, makeQName, qualifiers, unqualified
   , organize, folders, localItems, Hierarchy, F.toList
   ) where

import Data.Char
import Data.List hiding (insert)
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Foldable as F

data System = System 
   { systemName      :: String
   , systemHierarchy :: Hierarchy Module
   }
 deriving (Read, Show)

systemModules :: System -> [Module]
systemModules = F.toList . systemHierarchy

data Module = Module 
   { moduleFile      :: FilePath
   , moduleQName     :: QName
   , moduleImports   :: [QName]
   , moduleFunctions :: [Function]
   , moduleSize      :: !Size
   , moduleOverall   :: !Size
   }
 deriving (Read, Show)

data Function = Function
   { functionName :: String
   , functionSize :: !Size
   }
 deriving (Read, Show)

data Size = Size {sizeLines :: !Int, sizeChars :: !Int}
 deriving (Read, Show)
 
showSize :: Size -> String
showSize s = show (sizeLines s) ++ " lines (" ++ show (sizeChars s) ++ " chars)"

sizeOf :: String -> Size
sizeOf s = Size (length $ lines s) (length s)

class Sized a where
   size        :: a -> Size
   overallSize :: a -> Size
   -- default implementation
   overallSize = size

instance Sized Module where
   size        = moduleSize
   overallSize = moduleOverall

instance Sized Function where
   size = functionSize

instance Sized System where 
   size        = size . systemHierarchy
   overallSize = overallSize . systemHierarchy

instance Sized a => Sized (Hierarchy a) where
   size        = size . F.toList
   overallSize = overallSize . F.toList

instance Sized a => Sized [a] where
   size        = mconcat . map size
   overallSize = mconcat . map overallSize

overallLines, overallChars, linesOfCode, charsOfCode :: Sized a => a -> Int
overallLines = sizeLines . overallSize
overallChars = sizeChars . overallSize
linesOfCode  = sizeLines . size
charsOfCode  = sizeChars . size

instance Monoid Size where
   mempty = Size 0 0
   mappend (Size a1 a2) (Size b1 b2) = Size (a1+b1) (a2+b2)
   
data Hierarchy a = H (M.Map String (Hierarchy a)) [a]
 deriving (Read, Show)

instance Monoid (Hierarchy a) where
   mempty = H M.empty []
   mappend (H m1 as) (H m2 bs) = H (M.unionWith mappend m1 m2) (as++bs)
             
instance F.Foldable Hierarchy where
   foldMap f = rec 
    where
      rec (H m as) = mconcat (map rec (M.elems m) ++ map f as)

folders :: Hierarchy a -> [(String, Hierarchy a)]
folders (H m _) = M.toList m
 
localItems :: Hierarchy a -> [a]
localItems (H _ as) = as
 
singleton :: a -> Hierarchy a
singleton = H M.empty . return

folder :: String -> Hierarchy a -> Hierarchy a
folder s a = H (M.singleton s a) []

singletonAt :: [String] -> a -> Hierarchy a
singletonAt = flip (foldr folder . singleton)

insert :: [String] -> a -> Hierarchy a -> Hierarchy a
insert xs = mappend . singletonAt xs

organize :: (a -> [String]) -> [a] -> Hierarchy a 
organize f = foldr (\a -> insert (f a) a) mempty

class Named a where
   name  :: a -> String
   qname :: a -> QName
   -- default implementation
   name  = show . qname
   qname = Q [] . name

instance Named System where
   name = systemName

instance Named Module where
   qname = moduleQName
   
instance Named Function where
   name = functionName

data QName = Q { qualifiers :: [String], unqualified :: String }
   deriving (Eq, Ord)

instance Show QName where
   show qn = intercalate "." (qualifiers qn ++ [unqualified qn])

instance Read QName where
   readsPrec _ s = [ (makeQName xs, rest) | not (null xs) ]
    where (xs, rest) = span (\x -> isAlphaNum x || x == '.') s

makeQName :: String -> QName
makeQName s =
   case break (== '.') s of
      (x, [])   -> Q [] x
      (x, _:xs) -> let qn = makeQName xs 
                   in qn { qualifiers = x:qualifiers qn }