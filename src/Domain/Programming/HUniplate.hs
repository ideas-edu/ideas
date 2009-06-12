{-# LANGUAGE TypeOperators #-}

module Domain.Programming.Prog where

import Data.Dynamic
import Data.HList
import Data.Typeable

class HList l => HUniplate a l | l -> a, a -> l where -- perhaps Anyplate is a better name
  hUniplate :: a -> (l, l -> a)

instance HUniplate Module (Range :*:MaybeName :*: MaybeExports :*: Body  :*: HNil) where
  hUniplate (Module_Module r n e b) = ( r .*. n .*. e .*. b .*. HNil
                                      , \ (HCons r (HCons n (HCons e (HCons b HNil)))) -> 
                                          Module_Module r n e b
                                      )

class HChildren a l where
  hChildren :: a -> l
instance (HList l, HUniplate a l) => HChildren a l where
  hChildren = fst . hUniplate


class HUniverse a l where
  hUniverse :: a -> l
instance ( HList l
         , HConcatMap AppUniverse l' r
         , HChildren a l'
         , HExtend a r l
         ) => HUniverse a l where
  hUniverse x = x .*. hConcatMap AppUniverse (hChildren x)

data AppUniverse = AppUniverse
instance ( HList l
         , HConcatMap AppUniverse l' r
         , HChildren a l'
         , HExtend a r l
         ) => Apply AppUniverse a l where
  apply _ = hUniverse

--universe x = x : concatMap universe (children x ) 

data AppChildren = AppChildren
instance (HList l, HUniplate a l) => Apply AppChildren a l where 
  apply _ =  hChildren

data AppAppend = AppAppend
instance HAppend l l' l'' => Apply AppAppend (l, l') l'' where
  apply _ = uncurry hAppend

class HConcatMap f l r where
  hConcatMap :: f -> l -> r
instance (HFoldr AppAppend HNil l' r, HMap f l l') => HConcatMap f l r where
  hConcatMap f l = hFoldr AppAppend HNil $ hMap f l

class HZipWith f l1 l2 l3 where
   hZipWith :: f -> l1 -> l2 -> l3
instance (HZip l1 l2 l', HMap f l' l3) => HZipWith f l1 l2 l3 where
  hZipWith f l1 l2 = hMap f (hZip l1 l2)

data Add = Add
instance Num a => Apply Add (a,a) a where apply _ = uncurry (+)
s = hZipWith Add (HCons (1::Int) $ HCons (2::Double) HNil)
                 (HCons (3::Int) $ HCons (4::Double) HNil)



-- some test values

l' = 1 .*. 'a' .*. True .*. 'b' .*. HNil
l = m .*. HNil


-- A dynamic version of biplate... doesn't work
type Dyns = [Dynamic]

class Anyplate a where
  anyplate :: a -> (Dyns, Dyns -> a)

instance Anyplate Module where
  anyplate (Module_Module r n e b) = ( [toDyn r, toDyn n, toDyn e, toDyn b]
                                     , \ [r, n, e, b] -> Module_Module (fromDyn r noRange) (fromDyn n MaybeName_Nothing) 
                                                                       (fromDyn e MaybeExports_Nothing) (fromDyn b (Body_Body noRange [] []))
                                     )

aChildren :: Anyplate a => a -> Dyns
aChildren = fst . anyplate

{-
aUniverse :: (Typeable a, Anyplate a) => a -> Dyns
aUniverse x = toDyn x : concatMap (aUniverse) (fromDyns (aChildren x))
-}

fromDyns :: Typeable a => Dyns -> [a]
fromDyns = map fromJust . filter isJust . map fromDynamic
