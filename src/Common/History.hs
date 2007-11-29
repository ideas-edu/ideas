{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Common.History 
   ( HistoryT, History
   , evalHistory, runHistory, execHistory, evalHistoryT, runHistoryT, execHistoryT
   , undo, redo, mark, undoMarked, redoMarked
   , history, future, historyMarked, futureMarked
   ) where

import Control.Monad.State
import Control.Monad.Identity

data Info a = I 
   { infoHistory :: [(a, Bool)]
   , infoCurrent :: (a, Bool)
   , infoFuture  :: [(a, Bool)]
   }

makeInfo :: a -> Info a
makeInfo a = I { infoHistory=[], infoCurrent=(a, False), infoFuture=[] }

newtype HistoryT s m a = H { unH :: StateT (Info s) m a }
   deriving (Monad, MonadTrans, MonadIO)

type History s = HistoryT s Identity

instance Monad m => MonadState s(HistoryT s m) where
   get   = H $ gets (fst . infoCurrent)
   put s = H $ modify $ \info -> 
              info { infoHistory = (infoCurrent info) : infoHistory info, infoCurrent = (s, False), infoFuture = [] }

--------------------------------------------------------------------
-- special operations
           
undo, redo, mark, undoMarked, redoMarked :: Monad m => HistoryT s m ()   

undo = H $ modify $ \info -> 
   case infoHistory info of
      x:xs -> info {infoHistory=xs, infoCurrent=x, infoFuture=infoCurrent info:infoFuture info}
      _    -> info
             
redo = H $ modify $ \info -> 
   case infoFuture info of
      x:xs -> info {infoHistory=infoCurrent info:infoHistory info, infoCurrent=x, infoFuture=xs}
      _    -> info
             
mark = H $ modify $ \info ->
   info {infoCurrent=(fst (infoCurrent info), True)}    
  
undoMarked = H $ modify $ \info -> 
   case break snd (infoHistory info) of
      (xs, y:ys) -> info {infoHistory=ys, infoCurrent=y, infoFuture=reverse xs ++ infoFuture info}
      _    -> info
             
redoMarked = H $ modify $ \info -> 
   case break snd (infoFuture info) of
      (xs, y:ys) -> info {infoHistory=reverse xs ++ infoHistory info, infoCurrent=y, infoFuture=ys}
      _    -> info
      
history, future, historyMarked, futureMarked :: Monad m => HistoryT s m [s]   
history       = H $ gets $ map fst . infoHistory
future        = H $ gets $ map fst . infoFuture
historyMarked = H $ gets $ map fst . filter snd . infoHistory
futureMarked  = H $ gets $ map fst . filter snd . infoFuture
      
--------------------------------------------------------------------
-- eval/run/exec

evalHistory :: History s a -> s -> a
evalHistory h = fst . runHistory h

runHistory :: History s a -> s -> (a, s)
runHistory h = runIdentity . runHistoryT h

execHistory :: History s a -> s -> s
execHistory h = snd . runHistory h

evalHistoryT :: (Monad m) => HistoryT s m a -> s -> m a
evalHistoryT h s = runHistoryT h s >>= return . fst

runHistoryT :: Monad m => HistoryT s m a -> s -> m (a, s)
runHistoryT h s = do 
   (a, info) <- runStateT (unH h) (makeInfo s)
   return (a, fst (infoCurrent info))

execHistoryT :: (Monad m) => HistoryT s m a -> s -> m s
execHistoryT h s = runHistoryT h s >>= return . snd