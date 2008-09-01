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
   ( Reply(..), replyToXML, replyInXML
   , ReplyOk(..), ReplyIncorrect(..), ReplyError(..), Args
   ) where

import Common.Strategy hiding (not)
import OpenMath.StrategyTable
import OpenMath.Object
import Service.XML

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
   , repInc_Expected   :: OMOBJ
   , repInc_Derivation :: [(String, OMOBJ)]
   , repInc_Arguments  :: Args
   , repInc_Steps      :: Int
   , repInc_Equivalent :: Bool
   }
 deriving Show
 
data ReplyError = ReplyError
   { repErr_Kind    :: String
   , repErr_Message :: String
   }
 deriving Show

type Args = [(String, String)]

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

replyIncorrectToXML :: ReplyIncorrect -> XML
replyIncorrectToXML r = xmlResult "incorrect" $ xmlList (
   [ ("strategy",   Text $ repInc_Strategy r)
   , ("location",   Text $ show $ repInc_Location r)
   , ("expected",   omobj2xml $ repInc_Expected r)
   ] ++
   [ ("steps",      Text $ show $ repInc_Steps r)
   , ("equivalent", Text $ show $ repInc_Equivalent r)
   ]) ++ [ Tag "arguments" [] (map (\(x,y) -> Tag "elem" [("descr", x)] [Text y]) (repInc_Arguments r))
         | not (null $ repInc_Arguments r)
         ]
      ++ [ Tag "derivation" [] (map (\(x,y) -> Tag "elem" [("ruleid", x)] [omobj2xml y]) (repInc_Derivation r))
         | not (null $  repInc_Derivation r)
         ]

replyErrorToXML :: ReplyError -> XML
replyErrorToXML r = xmlResult (repErr_Kind r) [Text $ repErr_Message r]

xmlResult :: String -> [XML] -> XML
xmlResult result = Tag "reply" [("result", result), ("version", versionNr)]

xmlList :: [(String, XML)] -> [XML]
xmlList = map f
 where f (x, y) = Tag x [] [y]