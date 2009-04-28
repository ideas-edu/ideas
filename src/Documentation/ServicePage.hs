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
   bold $ text "Signature:"
   space
   case typedValue s of
      _ ::: t -> ttText (show t)
   h2 "XML request"
   pre $ text (XML.showXML (toRequest s))
 where
   title = "Service " ++ show (serviceName s)
   
toRequest :: Service a -> XML.XML
toRequest s = 
   case typedValue s of  
      _ ::: t -> XML.makeXML "request" (f t)
 where
   f t = mapM_ (\(Some a) -> XML.text $ show a) (arguments t)

arguments :: Type a t -> [Some (Type a)]
arguments (a :-> b) = Some a : arguments b
arguments _         = []