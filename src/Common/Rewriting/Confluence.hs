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
module Common.Rewriting.Confluence
   ( isConfluent, checkConfluence, checkConfluenceWith
   , somewhereM
   , Config, defaultConfig, showTerm, complexity, termEquality
   ) where

import Common.Id
import Common.Navigator
import Common.Rewriting.RewriteRule
import Common.Rewriting.Substitution
import Common.Rewriting.Term
import Common.Rewriting.Unification
import Common.Utils.Uniplate hiding (rewriteM)
import Data.Maybe

normalForm :: [RewriteRule a] -> Term -> Term
normalForm rs = run []
 where
   run hist a =
      case [ b | r <- rs, b <- somewhereM (rewriteTerm r) a ] of
         []   -> a
         hd:_ -> if hd `elem` hist
                 then error "cyclic"
                 else run (a:hist) hd

rewriteTerm :: RewriteRule a -> Term -> [Term]
rewriteTerm r t = do
   let lhs :~> rhs = ruleSpecTerm $
          case metaVars t of
             [] -> r
             ns -> renumberRewriteRule (maximum ns+1) r
   sub <- match lhs t
   return (sub |-> rhs)

-- uniplate-like helper-functions
somewhereM :: Uniplate a => (a -> [a]) -> a -> [a]
somewhereM f = concatMap leave . rec . navigator
 where
   rec ca = changeM f ca ++ concatMap rec (allDowns ca)

----------------------------------------------------

type Pair   a = (RewriteRule a, Term)
type Triple a = (RewriteRule a, Term, Term)

superImpose :: RewriteRule a -> RewriteRule a -> [Navigator Term]
superImpose r1 r2 = rec (navigator lhs1)
 where
    lhs1 :~> _ = ruleSpecTerm r1
    lhs2 :~> _ = ruleSpecTerm (renumber r1 r2)

    rec ca = case current ca of
                Just (TMeta _) -> []
                Just a -> maybe [] (return . (`subTop` ca)) (unify a lhs2) ++ concatMap rec (allDowns ca)
                Nothing -> []

    subTop s ca = fromMaybe ca $
       fmap (change (s |->)) (top ca) >>= navigateTo (location ca)

    renumber r = case metaInRewriteRule r of
                    [] -> id
                    xs -> renumberRewriteRule (maximum xs + 1)

criticalPairs :: [RewriteRule a] -> [(Term, Pair a, Pair a)]
criticalPairs rs =
   [ (a, (r1, b1), (r2, b2))
   | r1       <- rs
   , r2       <- rs
   , na <- superImpose r1 r2
   , compareId r1 r2 == LT || not (null (location na))
   , a  <- leave na
   , b1 <- rewriteTerm r1 a
   , b2 <- changeM (rewriteTerm r2) na >>= leave
   ]

noDiamondPairs :: Config -> [RewriteRule a] -> [(Term, Triple a, Triple a)]
noDiamondPairs cfg rs = noDiamondPairsWith (normalForm rs) cfg rs

noDiamondPairsWith :: (Term -> Term) -> Config -> [RewriteRule a] -> [(Term, Triple a, Triple a)]
noDiamondPairsWith f cfg rs =
   [ (a, (r1, e1, nf1), (r2, e2, nf2))
   | (a, (r1, e1), (r2, e2)) <- criticalPairs rs
   , let (nf1, nf2) = (f e1, f e2)
   , not (termEquality cfg nf1 nf2)
   ]

reportPairs :: Config -> [(Term, Triple a, Triple a)] -> IO ()
reportPairs cfg = putStrLn . unlines . zipWith report [1::Int ..]
 where
   f = showTerm cfg
   report i (a, (r1, e1, nf1), (r2, e2, nf2)) = unlines
      [ show i ++ ") " ++ f a
      , "  "   ++ showId r1
      , "    " ++ f e1 ++ if e1==nf1 then "" else "   -->   " ++ f nf1
      , "  "   ++ showId r2
      , "    " ++ f e2 ++ if e2==nf2 then "" else "   -->   " ++ f nf2
      ]

----------------------------------------------------

isConfluent :: [RewriteRule a] -> Bool
isConfluent = null . noDiamondPairs defaultConfig

checkConfluence :: [RewriteRule a] -> IO ()
checkConfluence = checkConfluenceWith defaultConfig

checkConfluenceWith :: Config -> [RewriteRule a] -> IO ()
checkConfluenceWith cfg = reportPairs cfg . noDiamondPairs cfg

data Config = Config
   { showTerm     :: Term -> String
   , complexity   :: Term -> Int
   , termEquality :: Term -> Term -> Bool
   }

defaultConfig :: Config
defaultConfig = Config show (const 0) (==)

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
r6 = rewriteRule "R6" $ \p -> p :||: T :~> F

r1, r2, r3 :: RewriteRule Expr
r1 = rewriteRule "a1" $ \a -> 0+a :~> a
r2 = rewriteRule "a3" $ \a b c -> a+(b+c) :~> (a+b)+c
r3 = rewriteRule "a2" $ \a -> a+0 :~> a

go = do -- putStrLn $ unlines $ map show $ criticalPairs [r1,r2]
        checkConfluence [r1,r2,r3]
-}