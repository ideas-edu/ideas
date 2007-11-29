module OpenMath.LAServer where

import Domain.LinearAlgebra
import OpenMath.OMToMatrix
import System.IO
import Text.XML.HaXml.Haskell2Xml

data Request a = Request (Matrix a) (Matrix a) StrategyID Location
   deriving Show

data Reply a = Ok 
             | Incorrect (Matrix a) MDQuestion StrategyID Location
   deriving Show

type StrategyID = String
type Location   = [Int]
type MDQuestion = String

main :: IO ()
main = do
   xml <- hGetContents stdin
   respond xml

----------------------------

-- A first attempt: can this be generated with HaXml?
{-
instance Haskell2Xml a => Haskell2Xml (Request a) where
   toHType (Request term answer strategy location) = 
      Tuple [toHType term, toHType answer, toHType strategy, toHType location]
   toContents (Request term answer strategy location) = 
      [CElem (Elem "request" [] (toContents term ++ toContents answer ++ toContents strategy ++ toContents location))]
   fromContents [CElem (Elem "request" [] cs)] =
      let (term,     cs1) = fromContents cs
          (answer,   cs2) = fromContents cs1
          (strategy, cs3) = fromContents cs2
          (location,  []) = fromContents cs3
      in (Request term answer strategy location, []) -}
  
----------------------------

respond :: String -> IO ()
respond = putStrLn . fromReply . laServer . toRequest

-- TODO
toRequest :: String -> Request Int
toRequest _ = Request mymatrix mymatrix "sid" []
 where mymatrix = makeMatrix [[0,1],[1,0]]
 
-- TODO
fromReply :: Reply Int -> String
fromReply = show

----------------------------

laServer :: Request a -> Reply a
laServer (Request term answer strategy location) = Ok