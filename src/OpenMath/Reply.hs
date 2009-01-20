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

import Control.Monad
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
replyOkToXML r = makeReply "ok" $ do
   element "strategy" (text $ repOk_Strategy r)
   element "location" (text $ show $ repOk_Location r)
   element "context"  (text $ repOk_Context r)
   element "steps"    (text $ show $ repOk_Steps r)

replyIncorrectToXML :: ReplyIncorrect -> XML
replyIncorrectToXML r = makeReply "incorrect" $ do
   element "strategy"   (text $ repInc_Strategy r)
   element "location"   (text $ show $ repInc_Location r)
   element "expected"   (builder $ omobj2xml $ repInc_Expected r)
   element "steps"      (text $ show $ repInc_Steps r)
   element "equivalent" (text $ show $ repInc_Equivalent r)
   
   unless (null $ repInc_Arguments r) $
       let f (x, y) = element "elem" $ do 
              "descr" .=. x 
              text y
       in element "arguments" $ mapM_ f (repInc_Arguments r)

   unless (null $  repInc_Derivation r) $
      let f (x,y) = element "elem" $ do 
             "ruleid" .=. x 
             builder (omobj2xml y)
      in element "derivation" $ mapM_ f (repInc_Derivation r)

replyErrorToXML :: ReplyError -> XML
replyErrorToXML r = makeReply (repErr_Kind r) (text $ repErr_Message r)
   
makeReply :: String -> XMLBuilder -> XML
makeReply kind body = makeXML "reply" $ do
   "result"  .=. kind
   "version" .=. versionNr
   body