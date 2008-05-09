-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module OpenMath.Reply 
   ( Reply(..), replyInXML
   , ReplyOk(..), ReplyIncorrect(..), ReplyError(..)
   ) where

import Common.Context (Location)
import Common.Strategy
import OpenMath.StrategyTable
import OpenMath.ObjectParser
import OpenMath.XML
import Data.Maybe

------------------------------------------------------------------------
-- Data types for replies

-- There are three possible replies: ok, incorrect, or an error in the protocol (e.g., a parse error)
data Reply = Ok ReplyOk | Incorrect ReplyIncorrect | Error ReplyError
   deriving Show

data ReplyOk = ReplyOk
   { repOk_Strategy :: StrategyID
   , repOk_Location :: StrategyLocation
   , repOk_Context  :: String
   , repOk_Steps    :: Int
   }
 deriving Show

data ReplyIncorrect = ReplyIncorrect
   { repInc_Strategy   :: StrategyID
   , repInc_Location   :: StrategyLocation
   , repInc_Expected   :: Expr
   , repInc_Arguments  :: Maybe String
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
 
replyInXML :: Reply -> String
replyInXML = showXML . replyToXML

replyToXML :: Reply -> XML
replyToXML reply =
   case reply of
      Ok r        -> replyOkToXML r
      Incorrect r -> replyIncorrectToXML r 
      Error r     -> replyErrorToXML r

replyOkToXML :: ReplyOk -> XML
replyOkToXML r = xmlResult "ok" $ xmlList
   [ ("strategy", Text $ repOk_Strategy r)
   , ("location", Text $ show $ repOk_Location r)
   , ("context",  Text $ repOk_Context r)
   , ("steps",    Text $ show $ repOk_Steps r)
   ]

-- For now, show a matrix with integers
replyIncorrectToXML :: ReplyIncorrect -> XML
replyIncorrectToXML r = xmlResult "incorrect" $ xmlList $
   [ ("strategy",   Text $ repInc_Strategy r)
   , ("location",   Text $ show $ repInc_Location r)
   , ("expected",   exprToXML $ repInc_Expected r)
   ] ++
   [ ("arguments",  Text $ fromMaybe "" $ repInc_Arguments r) 
   | isJust (repInc_Arguments r)
   ] ++
   [ ("steps",      Text $ show $ repInc_Steps r)
   , ("equivalent", Text $ show $ repInc_Equivalent r)
   ]

replyErrorToXML :: ReplyError -> XML
replyErrorToXML r = xmlResult (repErr_Kind r) [Text $ repErr_Message r]

xmlResult :: String -> [XML] -> XML
xmlResult result = Tag "reply" [("result", result), ("version", versionNr)]

xmlList :: [(String, XML)] -> [XML]
xmlList = map f
 where f (x, y) = Tag x [] [y]