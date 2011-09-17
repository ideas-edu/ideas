module Model 
   ( System(..), systemModules
   , Module(..), Function(..)
   , Size, sizeOf, sizeLines, sizeChars, Sized(..)
   , overallLines, overallChars, linesOfCode, charsOfCode
   , Named(..), QName, makeQName, qualifiers, unqualified
   , organize, folders, localItems, Hierarchy, F.toList
   ) where

import Control.Monad
import Data.List hiding (insert)
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Foldable as F
import Text.JSON

data System = System 
   { systemName      :: String
   , systemHierarchy :: Hierarchy Module
   }

instance InJSON System where
   toJSON (System a b) = toJSON (a, b)
   fromJSON = liftM (\(a, b) -> System a b) . fromJSON

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

instance InJSON Module where
   toJSON (Module a b c d e f) = 
      Array [toJSON a, toJSON b, toJSON c, toJSON d, toJSON e, toJSON f]
   fromJSON (Array [a1, a2, a3, a4, a5, a6]) = do
      (x1, x2, x3) <- liftM3 (,,) (fromJSON a1) (fromJSON a2) (fromJSON a3)
      (x4, x5, x6) <- liftM3 (,,) (fromJSON a4) (fromJSON a5) (fromJSON a6)
      return (Module x1 x2 x3 x4 x5 x6)
   fromJSON _ = fail "InJSON Module"

data Function = Function
   { functionName :: String
   , functionSize :: !Size
   }

instance InJSON Function where
   toJSON (Function a b) = toJSON (a, b)
   fromJSON = liftM (\(a, b) -> Function a b) . fromJSON 

data Size = Size {sizeLines :: !Int, sizeChars :: !Int}

instance Show Size where
   show s = show (sizeLines s) ++ " lines (" ++ show (sizeChars s) ++ " chars)"

instance InJSON Size where
   toJSON (Size a b) = toJSON (a, b)
   fromJSON = liftM (\(a, b) -> Size a b) . fromJSON 

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

instance Monoid (Hierarchy a) where
   mempty = H M.empty []
   mappend (H m1 as) (H m2 bs) = H (M.unionWith mappend m1 m2) (as++bs)
           
instance Functor Hierarchy where
   fmap f (H m as) = H (fmap (fmap f) m) (fmap f as)
             
instance F.Foldable Hierarchy where
   foldMap f = rec 
    where
      rec (H m as) = mconcat (map rec (M.elems m) ++ map f as)

instance InJSON a => InJSON (Hierarchy a) where
   toJSON (H m as) = Array [Object (M.toList (M.map toJSON m)), toJSON as]
   fromJSON (Array [Object xs, a]) = do
      let (ks, es) = unzip xs
      ys <- mapM fromJSON es
      as <- fromJSON a
      return (H (M.fromList (zip ks ys)) as)
   fromJSON _ = fail "InJSON (Hierarchy a)"

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

instance InJSON QName where
   toJSON   = toJSON . show
   fromJSON = liftM makeQName . fromJSON

makeQName :: String -> QName
makeQName s =
   case break (== '.') s of
      (x, [])   -> Q [] x
      (x, _:xs) -> let qn = makeQName xs 
                   in qn { qualifiers = x:qualifiers qn }