-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Ideas.Common.Rewriting.AutoTerm (toTermG, fromTermG, testTermFor) where

import Control.Monad.State
import Data.Data
import Data.List
import Ideas.Common.Rewriting.Term

toTermG :: Data a => a -> Term
toTermG a =
   case constrRep constr of
      IntConstr n   -> TNum n                  -- for Int and Integer
      FloatConstr r -> TFloat (fromRational r) -- for Double and Float
      CharConstr c  -> TVar [c]
      AlgConstr _   ->
         case cast a of
            Just s  -> TVar s -- for String
            Nothing -> makeTerm constr (gfoldl op e a)
 where
   op (M xs) x = M (xs ++ [toTermG x])
   e _    = M []
   constr = toConstr a

newtype M a = M [Term]

-- test for list constructors
makeTerm :: Constr -> M a -> Term
makeTerm c (M xs) =
   case xs of
      [y, TList ys] | isCons  -> TList (y:ys)
      []            | isNil   -> TList []
      _ -> TCon (constrSymbol c) xs
 where
   txt = showConstr c
   isNil   = txt == "[]"
   isCons  = txt == "(:)"

isTuple :: String -> Bool
isTuple ('(':xs) = rec xs
 where
   rec ")"      = True
   rec (',':ys) = rec ys
   rec _        = False
isTuple _       = False

------------------------------------------------------------------------

constrSymbol :: Constr -> Symbol
constrSymbol c
   | txt == "[]"  = nilSymbol
   | txt == "(:)" = consSymbol
   | isTuple txt  = tupleSymbol
   | otherwise    = newSymbol (dataTypeName (constrType c) `mappend` show c)
 where
   txt = showConstr c

nilSymbol, consSymbol, tupleSymbol :: Symbol
nilSymbol   = newSymbol "list.nil"
consSymbol  = newSymbol "list.cons"
tupleSymbol = newSymbol "tuple"

constructors :: Data a => Proxy a -> [Constr]
constructors = dataTypeConstrs . dataTypeOf . fromProxy
 where
   fromProxy :: Proxy a -> a
   fromProxy = error "fromProxy"

findConstr :: Data a => Proxy a -> Symbol -> Maybe Constr
findConstr p s = find (\c -> s == constrSymbol c) (constructors p)

fromTermG :: Data a => Term -> Maybe a
fromTermG term =
   case term of
      TCon s xs -> fromTermTConG Proxy s xs
      TVar [c]  -> cast c `mplus` cast [c]
      TVar s    -> cast s
      TList xs  -> fromTermG (foldr cons nil xs)
      TNum n    -> cast n `mplus` cast (fromInteger n :: Int)
      TFloat d  -> cast d `mplus` cast (doubleToFloat d)
      TMeta _   -> Nothing
 where
   cons = binary consSymbol
   nil  = symbol nilSymbol

doubleToFloat :: Double -> Float
doubleToFloat = fromRational . toRational

fromTermTConG :: Data a => Proxy a -> Symbol -> [Term] -> Maybe a
fromTermTConG p s xs = do
   c <- findConstr p s
   evalStateT (gunfold op return c) xs
 where
   op m = do
      f <- m
      t <- pop
      a <- lift (fromTermG t)
      return (f a)

pop :: StateT [a] Maybe a
pop = do
   ts <- get
   case ts of
      []    -> fail "pop"
      hd:tl -> put tl >> return hd

--------------

testTermFor :: (Data a, Eq a) => a -> Bool
testTermFor x = fromTermG (toTermG x) == Just x