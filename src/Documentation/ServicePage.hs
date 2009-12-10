-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Documentation.ServicePage (makeServicePage) where

import Documentation.DefaultPage
import Common.Utils (Some(..))
import Service.ServiceList
import Service.Types
import Text.HTML
import qualified Text.XML as XML

makeServicePage :: Service a -> IO ()
makeServicePage s =
   generatePage (servicePageFile s) (servicePage s)

servicePage :: Service a -> HTML
servicePage s = defaultPage title 1 $ do
   h1 (serviceName s)
   para $ do
      bold $ text "Signature:"
      space
      case serviceFunction s of
         _ ::: t -> ttText (show t)
   para $ do
      bold $ text "Description: "
      br
      text $ serviceDescription s
   h2 "XML request"
   pre $ text (XML.showXML (toRequest s))
 where
   title = "Service " ++ show (serviceName s)
   
toRequest :: Service a -> XML.XML
toRequest s = 
   case serviceFunction s of  
      _ ::: t -> XML.makeXML "request" (f t)
 where
   f = mapM_ (\ (Some a) -> XML.text $ show a) . arguments

arguments :: Type a t -> [Some (Type a)]
arguments (a :-> b) = Some a : arguments b
arguments _         = []