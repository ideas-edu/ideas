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
module Common.Rewriting.Confluence 
   ( isConfluent, checkConfluence, checkConfluenceWith
   ) where

import Common.Id
import Common.Rewriting.RewriteRule
import Common.Rewriting.Substitution
import Common.Rewriting.Unification
import Common.Rewriting.Term
import Common.Uniplate hiding (rewriteM)
import Control.Monad

unifyM :: Term -> Term -> [Substitution] -- temporary (partial) implementation
unifyM (Con s) (Con t) = [ emptySubst | s == t ]
unifyM (Num s) (Num t) = [ emptySubst | s == t ]
unifyM (Meta i) (Meta j) = [if i==j then emptySubst else singletonSubst i (Meta j)]
unifyM (Meta i) t = [ singletonSubst i t | not (i `hasMetaVar` t) ]
unifyM t (Meta i) = unifyM (Meta i) t
unifyM (Apply f a) (Apply g b) = unifyListM [f, a] [g, b]
unifyM (Apply _ _) (Con _) = []
unifyM (Con _) (Apply _ _) = []
unifyM (Apply _ _) (Num _) = []
unifyM (Num _) (Apply _ _) = []
unifyM x y = error ("unifyM: " ++ show (x, y))

unifyListM :: [Term] -> [Term] -> [Substitution]
unifyListM (x:xs) (y:ys) = do 
   s1 <- unifyM x y
   let f = map (s1 |->)
   s2 <- unifyListM (f xs) (f ys)
   return (s1 @@ s2)
unifyListM xs ys = do
   guard (null xs && null ys)
   return emptySubst

normalForm :: [RewriteRule a] -> Term -> Term
normalForm rs = run []
 where
   run hist a = 
      case [ b | r <- rs, b <- somewhereM (rewriteTerm r) a, b `notElem` hist ] of
         []   -> a
         hd:_ -> run (a:hist) hd 

rewriteTerm :: RewriteRule a -> Term -> [Term]
rewriteTerm r t = do
   let lhs :~> rhs = rulePair r
   sub <- match [] lhs t
   return (sub |-> rhs)

-- uniplate-like helper-functions
somewhereM :: (MonadPlus m, Uniplate a) => (a -> m a) -> a -> m a
somewhereM f a = msum $ f a : map g [0..n-1]
 where 
   n   = length (children a)
   g i = applyToM i (somewhereM f) a

applyToM :: (Monad m, Uniplate a) => Int -> (a -> m a) -> a -> m a
applyToM n f a = 
   let (as, build) = uniplate a 
       g (i, b) = if i==n then f b else return b
   in liftM build $ mapM g (zip [0..] as)

subtermsAt :: Uniplate a => a -> [([Int], a)]
subtermsAt a = ([], a) : [ (i:is, b) | (i, c) <- zip [0..] (children a), (is, b) <- subtermsAt c ]

applyAtM :: (Monad m, Uniplate a) => [Int] -> (a -> m a) -> a -> m a
applyAtM is f = foldr applyToM f is

----------------------------------------------------

type Pair   a = (RewriteRule a, Term)
type Triple a = (RewriteRule a, Term, Term)

superImpose :: RewriteRule a -> RewriteRule a -> [([Int], Term)]
superImpose r1 r2 =
   [ (loc, s |-> lhs2) | (loc, a) <- subtermsAt lhs2, s <- make a ]
 where
    lhs1 :~> _ = rulePair r1
    lhs2 :~> _ = rulePair (change r1 r2)
    
    make (Meta _) = []
    make a        = unifyM lhs1 a
    
    change r = case metaInRewriteRule r of
                  [] -> id
                  xs -> renumberRewriteRule (maximum xs + 1)

criticalPairs :: [RewriteRule a] -> [(Term, Pair a, Pair a)]
criticalPairs rs = 
   [ (a, (r1, b1), (r2, b2)) 
   | r1       <- rs
   , r2       <- rs
   , (loc, a) <- superImpose r1 r2
   , b1       <- rewriteTerm r1 a
   , b2       <- applyAtM loc (rewriteTerm r2) a
   , b1 /= b2 
   ]

noDiamondPairs :: [RewriteRule a] -> [(Term, Triple a, Triple a)]
noDiamondPairs rs = noDiamondPairsWith (normalForm rs) rs

noDiamondPairsWith :: (Term -> Term) -> [RewriteRule a] -> [(Term, (RewriteRule a, Term, Term), (RewriteRule a, Term, Term))]
noDiamondPairsWith f rs =
   [ (a, (r1, e1, nf1), (r2, e2, nf2)) 
   | (a, (r1, e1), (r2, e2)) <- criticalPairs rs
   , let (nf1, nf2) = (f e1, f e2)
   , nf1 /= nf2
   ]

reportPairs :: (Term -> String) -> [(Term, Triple a, Triple a)] -> IO ()
reportPairs f = putStrLn . unlines . zipWith report [1::Int ..]
 where
   report i (a, (r1, e1, nf1), (r2, e2, nf2)) = unlines
      [ show i ++ ") " ++ f a
      , "  "   ++ showId r1
      , "    " ++ f e1 ++ if e1==nf1 then "" else "   -->   " ++ f nf1
      , "  "   ++ showId r2
      , "    " ++ f e2 ++ if e2==nf2 then "" else "   -->   " ++ f nf2
      ]

----------------------------------------------------

isConfluent :: [RewriteRule a] -> Bool
isConfluent = null . noDiamondPairs

checkConfluence :: [RewriteRule a] -> IO ()
checkConfluence = checkConfluenceWith show

checkConfluenceWith :: (Term -> String) -> [RewriteRule a] -> IO ()
checkConfluenceWith f = reportPairs f . noDiamondPairs 

----------------------------------------------------
-- Example
{-
r1, r2, r3, r4, r5 :: RewriteRule SLogic
r1 = rewriteRule "R1" $ \p q r -> p :||: (q :||: r) :~> (p :||: q) :||: r 
r2 = rewriteRule "R2" $ \p q   -> p :||: q :~> q :||: p
r3 = rewriteRule "R3" $ \p     -> p :||: p :~> p
r4 = rewriteRule "R4" $ \p     -> p :||: T :~> T
r5 = rewriteRule "R5" $ \p     -> p :||: F :~> p

this = [r1, r2, r3, r4, r5, r6]
go = reportPairs $ noDiamondPairs this

r6 :: RewriteRule SLogic
r6 = rewriteRule "R6" $ \p -> p :||: T :~> F -}