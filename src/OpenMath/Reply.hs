module OpenMath.Reply 
   ( Reply(..), xmlReply
   , ReplyOk(..), ReplyIncorrect(..), ReplyError(..)
   ) where

import OpenMath.StrategyTable
import OpenMath.OMToMatrix
import Domain.LinearAlgebra
import Data.List

------------------------------------------------------------------------
-- Data types for replies

-- There are three possible replies: ok, incorrect, or an error in the protocol (e.g., a parse error)
data Reply = Ok ReplyOk | Incorrect ReplyIncorrect | Error ReplyError
   deriving Show

data ReplyOk = ReplyOk
   { repOk_Strategy :: StrategyID
   , repOk_Location :: Location
   , repOk_Steps    :: Int
   }
 deriving Show

data ReplyIncorrect = ReplyIncorrect
   { repInc_Strategy   :: StrategyID
   , repInc_Location   :: Location
   , repInc_Expected   :: Matrix Rational
   , repInc_Steps      :: Int
   , repInc_Equivalent :: Bool
   }
 deriving Show
 
data ReplyError = ReplyError
   { repErr_Kind    :: String
   , repErr_Message :: String
   }
 deriving Show

------------------------------------------------------------------------
-- Conversion functions to XML
 
xmlReply :: Reply -> String
xmlReply reply =
   case reply of
      Ok r        -> xmlReplyOk r
      Incorrect r -> xmlReplyIncorrect r 
      Error r     -> xmlReplyError r

xmlReplyOk :: ReplyOk -> String
xmlReplyOk r = xmlResult "ok" $ xmlList
   [ ("strategy", repOk_Strategy r)
   , ("location", show $ repOk_Location r)
   , ("steps",    show $ repOk_Steps r)
   ]

-- For now, show a matrix with integers
xmlReplyIncorrect :: ReplyIncorrect -> String
xmlReplyIncorrect r = xmlResult "incorrect" $ xmlList
   [ ("strategy",   repInc_Strategy r)
   , ("location",   show $ repInc_Location r)
   , ("expected",   matrix2xml $ fmap round $ repInc_Expected r)
   , ("steps",      show $ repInc_Steps r)
   , ("equivalent", show $ repInc_Equivalent r)
   ]

xmlReplyError :: ReplyError -> String
xmlReplyError r = xmlResult (repErr_Kind r) [repErr_Message r]

xmlResult :: String -> [String] -> String
xmlResult result = unlines . tagAttr "reply" [("result", result)]

xmlList :: [(String, String)] -> [String]
xmlList = concatMap f
 where f (x, y) = tag x [y]
 
-------------------------------------------------------------------------
-- Formatting utility functions

type Attrs = [(String, String)]

tag :: String -> [String] -> [String]
tag t = tagAttr t []

tagAttr :: String -> Attrs -> [String] -> [String]
tagAttr t attrs xs = [openTagAttr t attrs] ++ indent 3 xs ++ [closeTag t]

openTag, closeTag :: String -> String
openTag  t = "<"  ++ t ++ ">"
closeTag t = openTag ("/" ++ t)

openTagAttr :: String -> Attrs -> String
openTagAttr t attrs = openTag (concat $ intersperse " " $ t : map f attrs)
 where f (x, y) = x ++ "=" ++ show y
 
indent :: Int -> [String] -> [String]
indent n = map (replicate n ' '++)