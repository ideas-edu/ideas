{-# OPTIONS -XExistentialQuantification -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances -XUndecidableInstances -XTypeSynonymInstances #-}
module Common.Rewriting.Term 
   ( Rewrite(..), ShallowEq(..), IsTerm(..), Different(..)
   , RewriteRule(ruleName, rulePair)
   , Builder, BuilderList
   , RuleSpec(..)
   , match, rewriteM, rewriteRule, rewriteRules
   , smartGenerator
   , Term(..), spine, isBin
   ) where

import Common.Utils (safeHead)
import Common.Uniplate hiding (rewrite, rewriteM)
import Common.Rewriting.AC
import Control.Monad
import Data.Maybe
import Test.QuickCheck
import Common.Apply

infixl 1 :~>

class ShallowEq a where 
   shallowEq :: a -> a -> Bool

-- The arbitrary type class is a quick solution to have smart generators
-- (in combination with lifting rules). The function in the RewriteRule module
-- cannot have a type class for this reason
-- The show type class is added for pretty-printing rules
class (Arbitrary a, Show a, IsTerm a, Uniplate a, ShallowEq a) => Rewrite a where
   operators :: [Operator a]
   -- default definition: no associative/commutative operators
   operators = []

data Term = Var String | Con String | App Term Term | Num Integer | Meta Int
  deriving (Show, Eq, Ord)
  
instance Uniplate Term where
   uniplate term =
      case term of
         Var _   -> ([], \_ -> term)
         Con _   -> ([], \_ -> term)
         App f a -> ([f,a], \[g,b] -> App g b)
         Num _   -> ([], \_ -> term)
         Meta _  -> ([], \_ -> term)
         
instance ShallowEq Term where
   shallowEq a b =
      case (a, b) of
         (Var s,   Var t  ) -> s==t
         (Con s,   Con t  ) -> s==t
         (App _ _, App _ _) -> True
         (Num a,   Num b  ) -> a==b
         (Meta a,  Meta b ) -> a==b
         _                  -> False

instance IsTerm String where
   toTerm = Var
   fromTerm (Var s) = return s
   fromTerm _ = Nothing

class IsTerm a where
   toTerm :: a -> Term
   fromTerm :: Term -> Maybe a
   
class Different a where
   different :: (a, a)
   
data RuleSpec a = a :~> a deriving Show

data RewriteRule a = Rewrite a => R { ruleName :: String, nrOfMetaVars :: Int, rulePair :: Int -> RuleSpec Term }

class Builder t a | t -> a where
   buildSpec :: t -> Int -> RuleSpec Term
   countVars :: t -> Int

instance IsTerm a => Builder (RewriteRule a) a where
   buildSpec = rulePair
   countVars = nrOfMetaVars

instance IsTerm a => Builder (RuleSpec a) a where
   buildSpec rp _ = pr rp
   countVars _    = 0

instance (Different a, Builder t b) => Builder (a -> t) b where
   buildSpec f i = fn i (\a -> buildSpec (f a) (i+1))
   countVars f   = countVars (f $ error "countVars") + 1

class BuilderList t a | t -> a where
   getSpecNr   :: t -> Int -> Int -> RuleSpec Term
   countSpecsL :: t -> Int
   countVarsL  :: t -> Int

instance Rewrite a => BuilderList (RewriteRule a) a where
   getSpecNr r n = if n==0 then rulePair r else error "getSpecNr"
   countSpecsL _ = 1
   countVarsL    = nrOfMetaVars
 
instance Builder t a => BuilderList [t] a where
   getSpecNr rs = buildSpec . (rs !!)
   countSpecsL  = length
   countVarsL _ = 0

instance (Different a, BuilderList t b) => BuilderList (a -> t) b where 
   getSpecNr f n i = fn i (\a -> getSpecNr (f a) n (i+1))
   countSpecsL f   = countSpecsL (f $ error "countSpecsL")
   countVarsL f    = countVarsL (f $ error "countSpecsL") + 1

pr :: IsTerm a => RuleSpec a -> RuleSpec Term
pr (a :~> b) = toTerm a :~> toTerm b

fn :: Different a => Int -> (a -> RuleSpec Term) -> RuleSpec Term
fn n f = fill n a1 a2 :~> fill n b1 b2
 where
   a1 :~> b1 = f (fst different)
   a2 :~> b2 = f (snd different)

fill :: Int -> Term -> Term -> Term
fill i (App a1 a2) (App b1 b2) = App (fill i a1 b1) (fill i a2 b2)
fill i a b 
   | shallowEq a b = a
   | otherwise     = Meta i

type Sub = [(Int, Term)]

match :: Operators Term -> Term -> Term -> [Sub]
match ops (Meta i) (Meta j) | i==j = return []
match ops (Meta i) a | i `notElem` metas a = return [(i, a)]
match ops a (Meta i) | i `notElem` metas a = return [(i, a)]
match ops a@(App _ _) b@(App _ _) | isJust mop = 
   let Just op = mop
       pairs   = pairingsMatch op a b
   in concatMap (\ts -> uncurry (matchList ops) (unzip ts)) pairs
 where mop = findOperator ops a
match ops (App a1 a2) (App b1 b2) = do
   s1 <- match ops a1 b1
   s2 <- match ops (s1 |-> a2) (s1 |-> b2)
   return (s1 ++ s2)
match ops a b 
   | shallowEq a b = return []
   | otherwise     = fail "no match"

matchList :: Operators Term -> [Term] -> [Term] -> [Sub]
matchList ops [] [] =  return []
matchList ops (a:as) (b:bs) = do
   s1 <- match ops a b
   s2 <- matchList ops (map (s1 |->) as) (map (s1 |->) bs)
   return (s1 ++ s2)
matchList _ _ _ = fail "no match"

(|->) :: Sub -> Term -> Term
sub |-> Meta i  = fromMaybe (Meta i) (lookup i sub)
sub |-> App f a = App (sub |-> f) (sub |-> a)
_   |-> a       = a

metas :: Term -> [Int]
metas a = [ i | Meta i <- universe a ]

buildTerm :: Operators Term -> RuleSpec Term -> Term -> [Term]
buildTerm ops (lhs :~> rhs) a = do
   s <- match ops lhs a
   return (s |-> rhs)
   
build :: Rewrite a => RuleSpec Term -> a -> [a]
build p a = buildTerm (map convOp (getOp a)) p (toTerm a) >>= (maybe [] return . fromTerm)
 where
   getOp :: Rewrite a => a -> Operators a
   getOp _ = operators

rewriteRule :: (Builder f a, Rewrite a) => String -> f -> RewriteRule a
rewriteRule s f = R s (countVars f) (buildSpec f)

rewriteRules :: (BuilderList f a, Rewrite a) => String -> f -> [RewriteRule a]
rewriteRules s f = map (R s (countVarsL f) . getSpecNr f) [0 .. countSpecsL f-1]

rewrite :: RewriteRule a -> a -> [a]
rewrite r@(R _ _ _) a = do
   ext <- extendContext operators r
   build (rulePair ext 0) a

rewriteM :: MonadPlus m => RewriteRule a -> a -> m a
rewriteM r = msum . map return . rewrite r

------------------------------------------------------

-- Bug fix 4/3/2009: for associative operators, we need to extend rewrite
-- rules to take "contexts" into account. In addition to a left and a right
-- context, we also should consider a context on both sides. If not, we 
-- might miss some locations, as pointed out by Josje's bug report.
extendContext :: Operators a -> RewriteRule a -> [RewriteRule a]
extendContext ops r@(R _ _ _) =
   case findOp2 ops (lhs $ rulePair r 0) of
      Just op | isAssociative op -> 
         [r, extend (leftContext op) r, extend (rightContext op) r 
         , extend (rightContext op) (extend (leftContext op) r) ]
      _ -> [r]
 where
   leftContext op a (x :~> y) =
      constructor op a x :~> constructor op a y
   
   rightContext op a (x :~> y) =
      constructor op x a :~> constructor op y a

extend :: (Term -> RuleSpec Term -> RuleSpec Term) -> RewriteRule a -> RewriteRule a
extend f (R s n g) = R s (n+1) (\i -> f (Meta (i+n)) (g i))
      
lhs (a :~> _) = a

findOp2 :: IsTerm a => Operators a -> Term -> Maybe (Operator Term)
findOp2 [] _ = Nothing
findOp2 (o:os) a = 
   case findOperator [convOp o] a of
      Just _  -> Just (convOp o)
      Nothing -> findOp2 os a

------------------------------------------------------

spine :: Term -> (Term, [Term])
spine = rec []
 where
   rec xs (App f a) = rec (a:xs) f
   rec xs a         = (a, xs)

isBin :: String -> Term -> Maybe (Term, Term)
isBin s term =
   case spine term of
      (Con t, [a, b]) | s==t -> Just (a, b)
      _ -> Nothing

bin :: String -> Term -> Term -> Term
bin s = App . App (Con s)

collectAssociative :: String -> Term -> [Term]
collectAssociative s = 
   let rec term = maybe (term:) f (isBin s term)
       f (a, b) = rec a . rec b
   in ($ []) . rec

plusOp :: Operator Term
plusOp = associativeOperator (bin ":+:") (isBin ":+:")

convOp :: IsTerm a => Operator a -> Operator Term
convOp op = op 
   { constructor = bin s
   , destructor  = isBin s
   }
 where
   s = case spine (toTerm (constructor op undefined undefined)) of
          (Con s, [_,_]) -> s
          _ -> ""

instance Apply RewriteRule where 
   applyAll = rewrite

-----------------------------------------------------------
-- Smart generator that creates instantiations of the left-hand side

smartGenerator :: RewriteRule a -> Gen a
smartGenerator r@(R _ _ _) = do 
   let a :~> _ = rulePair r 0
   let vs      = metas a
   list <- vector (length vs) 
   let sub = zip vs (map (tpToTerm r) list)
   case fromTerm (sub |-> a) of
      Just a  -> return a
      Nothing -> arbitrary
 where
   tpToTerm :: IsTerm a => RewriteRule a -> a -> Term
   tpToTerm _ = toTerm

------------------------------------------------------
{-
data Equality a = a :==: a deriving Show
data Expr = Expr :+: Expr | I Integer deriving Show

instance IsTerm a => IsTerm (Equality a) where
   toTerm (a :==: b) = App (App (Con ":==:") (toTerm a)) (toTerm b)
   fromTerm (App (App (Con ":==:") a) b) =
      liftM2 (:==:) (fromTerm a) (fromTerm b)
   fromTerm _ = Nothing

instance Arbitrary Expr
instance Arbitrary (Equality a)

instance Different Expr where different = (I 0, I 1)

instance Different Integer where different = (0, 1)

instance IsTerm Expr where
   toTerm (a :+: b) = App (App (Con ":+:") (toTerm a)) (toTerm b)
   toTerm (I n) = Num n
   
   fromTerm (App (App (Con ":+:") a) b) =
      liftM2 (:+:) (fromTerm a) (fromTerm b)
   fromTerm (Num n) = Just (I n)
   fromTerm _ = Nothing

go f = fn 0 (\a -> fn 1 (\b -> pr (f a b)))

r1 :: RewriteRule (Equality Expr)
r1 = rewriteRule "r1" $ \a b -> ((a :+: I 5) :==: I b) :~> (a :==: a)

r2 :: RewriteRule Expr
r2 = rewriteRule "r2" $ \a b -> (a :+: b) :~> (b :+: a)

  
testje :: Maybe Expr
testje = rewriteM r2 $
   (I 4 :+: I 5 :+: I 7) -}