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
module Text.OpenMath.Reply 
   ( Reply(..), replyToXML, replyInXML
   , ReplyOk(..), ReplyIncorrect(..), ReplyError(..), Args
   ) where

import Control.Monad
import Common.Exercise
import Common.Strategy hiding (not)
import Domain.Math.Expr
import Text.OpenMath.Object
import Text.XML
import Service.Revision

------------------------------------------------------------------------
-- Data types for replies

-- There are three possible replies: ok, incorrect, or an error in the protocol (e.g., a parse error)
data Reply a = Ok (ReplyOk a) | Incorrect (ReplyIncorrect a) | Error ReplyError

data ReplyOk a = ReplyOk
   { repOk_Code     :: Exercise a
   , repOk_Location :: StrategyLocation
   , repOk_Context  :: String
   , repOk_Steps    :: Int
   }
   
data ReplyIncorrect a = ReplyIncorrect
   { repInc_Code       :: Exercise a
   , repInc_Location   :: StrategyLocation
   , repInc_Expected   :: a
   , repInc_Derivation :: [(String, a)]
   , repInc_Arguments  :: Args
   , repInc_Steps      :: Int
   , repInc_Equivalent :: Bool
   }
 
data ReplyError = ReplyError
   { repErr_Kind    :: String
   , repErr_Message :: String
   }

type Args = [(String, String)]

------------------------------------------------------------------------
-- Conversion functions to XML
 
replyInXML :: IsExpr a => Reply a -> String
replyInXML = showXML . replyToXML

replyToXML :: IsExpr a => Reply a -> XML
replyToXML reply =
   case reply of
      Ok r        -> replyOkToXML r
      Incorrect r -> replyIncorrectToXML r 
      Error r     -> replyErrorToXML r

replyOkToXML :: ReplyOk a -> XML
replyOkToXML r = makeReply "ok" $ do
   element "strategy" (text $ show $ exerciseCode $ repOk_Code r)
   element "location" (text $ show $ repOk_Location r)
   element "context"  (text $ repOk_Context r)
   element "steps"    (text $ show $ repOk_Steps r)

replyIncorrectToXML :: IsExpr a => ReplyIncorrect a -> XML
replyIncorrectToXML r = makeReply "incorrect" $ do
   element "strategy"   (text $ show $ exerciseCode $ repInc_Code r)
   element "location"   (text $ show $ repInc_Location r)
   element "expected"   (builder $ omobj2xml $ toOMOBJ $ toExpr $ repInc_Expected r)
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
             builder (omobj2xml (toOMOBJ (toExpr y)))
      in element "derivation" $ mapM_ f (repInc_Derivation r)

replyErrorToXML :: ReplyError -> XML
replyErrorToXML r = makeReply (repErr_Kind r) (text $ repErr_Message r)
   
makeReply :: String -> XMLBuilder -> XML
makeReply kind body = makeXML "reply" $ do
   "result"  .=. kind
   "version" .=. version
   body