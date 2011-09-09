module Main (main) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Model
import Scanner
import qualified Data.Map as M
import System.Environment
import Text.JSON

go = report "../../src"

main :: IO ()
main = do
   xs <- getArgs
   case xs of 
      [file] -> report file
      _      -> return ()

report :: FilePath -> IO ()
report file = do
   sys <- scanSystem file
   let ms = systemModules sys

   putStrLn $ showTable id (["System size"], 
      [ ("system",  name sys)
      , ("modules", show (length (systemModules sys)))
      , ("code size", showSize (size sys))
      , ("full size", showSize (overallSize sys))
      ])
      
   putStrLn $ showTable show $ (["Package size (LOC)"], 
      [ (s, n)
      | let spaces n = replicate n ' '
            collect i a = concat 
               [ (spaces (i*2) ++ s, linesOfCode b) : collect (i+1) b 
               | (s, b) <- folders a 
               ]
      , (s, n) <- collect 0 (systemHierarchy sys)
      ])
      
   putStrLn $ showTable show $ limitTable 20 $ addAvgInt $ sortTable (["Largest modules (LOC)"], 
      [ (name m, linesOfCode m)
      | m <- ms
      ])
      
   putStrLn $ showTable show $ limitTable 20 $ addAvgInt $ sortTable (["Largest functions (LOC)"], 
      [ (name f ++ "  (" ++ name m ++ ")", linesOfCode f)
      | m <- ms, f <- moduleFunctions m
      ])
      
   putStrLn $ showTable show $ limitTable 20 $ addAvgInt $ sortTable (["Most imports"], 
      [ (name m, length (moduleImports m))
      | m <- ms
      ])
      
   writeFile "output/locs-data.js" $ 
      "var moduleLOCS =" ++ show (moduleLOCS sys) ++ ";"
      
   writeFile "output/imports-data.js" $ 
      let (labs, dat) = modImports sys
      in "var labs =" ++ show labs ++ ";\n" ++
         "var data = " ++ show dat ++ ";"

   writeFile "output/model.txt" (show sys)

   --dot -Tsvg -O *.dot
   let rec pr h = 
          let f (s, a) = rec (pr++[s]) a
          in (pr, h) : concatMap f (folders h)
   forM_ (rec [] (systemHierarchy sys)) $ \(pr, h) -> do
      print (importsFile pr)
      writeFile (importsFile pr) (impGraph pr h sys)     
 
moduleLOCS :: System -> JSON
moduleLOCS = rec . systemHierarchy
 where
   rec a = Object $
      [ (s, rec b) | (s, b) <- folders a ] ++
      [ (lab m, toJSON (linesOfCode m)) | m <- localItems a ]
    where
      xs = map fst (folders a)
      lab m = let x = unqualified (qname m)
              in if x `elem` xs then x ++ ".hs" else x

modImports :: System -> (JSON, JSON)
modImports sys = (Array ms, Array is)
 where
   (ms, is) = unzip [ (toJSON (name m), toJSON (length (moduleImports m)))
                    | m <- systemModules sys 
                    ]

type Table a = ([String], [(String, a)])

sortTable :: Ord a => Table a -> Table a
sortTable (title, xs) = (title, sortBy cmp xs)
 where
   cmp (_, x) (_, y) = y `compare` x

limitTable :: Int -> Table a -> Table a
limitTable n (title, xs) = (title, take n xs)

showTable :: (a -> String) -> Table a -> String
showTable f (title, xs) = unlines (header ++ body)
 where
   header = title ++ [line]
   body   = map pp xs
   line   = replicate len '-'
   pp (a, b) = alignl n a ++ " : " ++ f b
   n   = maximum (0 : map (length . fst) xs)
   len = maximum (map length (title++body))

addAvgInt :: Table Int -> Table Int
addAvgInt (title, xs) = (title++[stats], xs)
 where
   (a, sd) = avgStdev (map snd xs)
   stats   = "   (avg=" ++ showDouble 2 a ++ ", stdev=" ++ showDouble 2 sd ++ ")"

avgStdev :: [Int] -> (Double, Double)
avgStdev xs 
   | null xs   = (0, 0)
   | otherwise = (avg, stdev)
 where
   len   = fromIntegral (length xs)
   ys    = map fromIntegral xs
   avg   = sum ys / len
   stdev = sqrt (sum [ (y-avg)^2 | y <- ys ] / len)

showDouble :: Int -> Double -> String
showDouble n = show . precision n 

precision :: Int -> Double -> Double
precision n = (/a) . fromInteger . round . (*a)
 where a = 10 Prelude.^ max 0 n


reportFunctionLOC :: [Function] -> IO ()
reportFunctionLOC funs = putStrLn $ unlines $
      [ intercalate "," (map (show . g) xs)
      | f <- [f5] -- [f1, f2, f3, f4]
      , let xs = f $ map linesOfCode $ funs
      , g <- [{-fst,-} snd]
      ]
 where
   f1 = complete . origin . histogram
   f2 = cumulative . complete . origin . histogram
   f3 = weighted . complete . origin . histogram
   f4 = cumulative . weighted . complete . origin . histogram
   f5 = ratio . cumulative . weighted . complete . origin . histogram

histogram :: Ord a => [a] -> [(a, Int)]
histogram = map f . group . sort
 where
   f xs@(a:_) = (a, length xs)

complete :: [(Int, Int)] -> [(Int, Int)]
complete (a:b:rest) = a:[ (n,0) | n <- [fst a+1..fst b-1] ]++complete (b:rest)
complete xs = xs

origin :: [(Int, Int)] -> [(Int, Int)]
origin xs = (0,0) : xs

cumulative :: [(a, Int)] -> [(a, Int)] 
cumulative = rec 0 
 where
   rec _ [] = []
   rec n ((a, i):xs) = (a, n+i): rec (n+i) xs

weighted :: [(Int, Int)] -> [(Int, Int)]
weighted = map (\(a, b) -> (a, a*b))

ratio :: [(Int, Int)] -> [(Int, Double)]
ratio xs = map (\(a, b) -> (a, fromIntegral (10000*b `div` n)/10000)) xs
 where n = snd $ last xs

alignr :: Int -> String -> String
alignr n s = replicate (n - length s) ' ' ++ s

alignl :: Int -> String -> String
alignl n s = take n (s ++ repeat ' ')

line :: String
line = replicate 75 '-'

importsFile :: [String] -> String
importsFile prefix = intercalate "-" ("imports":prefix) ++ ".dot"

impGraph :: [String] -> Hierarchy Module -> System -> String 
impGraph prefix hier sys = show $ href (importsFile [] ++ ".svg") $ 
   sub `addTo` (graph (name sys) nodes3 (doubles edges2)) 
 where
   allms = systemModules sys 
   ms = toList hier
   ms1 = localItems hier
   ms2 = concatMap (toList . snd) (folders hier)
   level = length prefix + 1
   ints  = map ((prefix++) . return . fst) (folders hier)
   
   sub = (filled $ color "lightgrey" $ href "" $ graph (intercalate "." prefix) nodes edges1) 
   nodes = nodes1 ++ nodes2
   nodes1 = [ filled $ color "white" $ label (unqualified (qname n)) $
              (node (name n)) 
            | n <- ms1
            ]
   nodes2 = nub $ 
            [ filled $ color "yellow" $ shape "rectangle" $ 
              tooltip sn $ label sn $ 
              href (importsFile n ++ ".svg") $
              (node ("I+" ++ intercalate "." n)) 
            | n <- ints 
            , let pr = intercalate "." n ++ "."
            , let i  = length (filter (isPrefixOf pr . name) allms)
            , let sn = show (last n ++ " (" ++ show i ++ ")")
            ]
   nodes3 = [ if n `elem` map name allms 
              then (node n)
              else filled $ color "yellow" $ shape "rectangle" $
                   href (importsFile (qname2 n) ++ ".svg") $
                   (node n)
            | m <- ms, i <- moduleImports m
            , i `notElem` map qname ms
            , i `elem` map qname allms
            , let n = f i
            ]
   nodes4 = [ node (show i)
            | i <- nub (concatMap moduleImports ms)
            , show i `notElem` map name allms
            ]
        
   sub2 = graph "external" nodes4 []    
   edges1 = doubles [ edge n1 n2
                    | m <- ms, i <- moduleImports m
                    , i `elem` map qname ms
                    , let n1 = f (qname m)
                    , let n2 = f i
                    , n1 /= n2
                    ]
   edges2 = doubles [ dotted $ color "grey" (edge n1 n2)
                    | m <- ms, i <- moduleImports m
                    , i `notElem` map qname ms
                    , i `elem` map qname allms
                    , let n1 = f (qname m)
                    , let n2 = f i
                    , n1 /= n2
                    ]
   f i | i `elem` map qname ms1 = show i
       | i `elem` map qname ms2 = ("I+"++) $ intercalate "." $ take level $ qname2 (show i)
       | otherwise = g (qname2 (show i)) prefix
       
   g (a:as) (b:bs) 
      | a == b    = a ++ "." ++ g as bs
      | otherwise = a
   g [] [] = []
   g _ _ = undefined

   
qname2 :: String -> [String]
qname2 xs =
   case break (== '.') xs of
      (x, [])   -> [x]
      (x, _:xs) -> x : qname2 xs
      
data Graph a = Graph
   { nodes      :: [Node a]
   , edges      :: [Edge a]
   , subgraphs  :: [Graph a]
   , gProps     :: Properties
   }
   
data Node a = Node 
   { nValue   :: a
   , nProps   :: Properties
   } deriving Eq

data Edge a = Edge 
   { from   :: a 
   , to     :: a
   , eProps :: Properties
   }

type Properties = M.Map String String

class Property a where
   setProp :: String -> String -> a -> a

instance Property (Graph a) where
   setProp p a g = g {gProps = M.insert p a (gProps g)}

instance Property (Node a) where
   setProp p a n = n {nProps = M.insert p a (nProps n)}
   
instance Property (Edge a) where
   setProp p a e = e {eProps = M.insert p a (eProps e)}
   
color, shape, label, tooltip, href :: Property a => String -> a -> a
color   = setProp "color"
shape   = setProp "shape"
label   = setProp "label"
tooltip = setProp "tooltip"
href    = setProp "href" . show

filled, dotted, bothWays :: Property a => a -> a
filled   = setProp "style" "filled"
dotted   = setProp "style" "dotted"
bothWays = setProp "dir" "both"

graph :: String -> [Node a] -> [Edge a] -> Graph a
graph s ns es = label (show s) $ tooltip (show s) $ 
   Graph ns es [] M.empty

addTo :: Graph a -> Graph a -> Graph a
addTo sub g = g {subgraphs = sub : subgraphs g}

node :: a -> Node a
node a = Node a M.empty

edge :: a -> a -> Edge a
edge a b = Edge a b M.empty

doubles :: Eq a => [Edge a] -> [Edge a]
doubles [] = []
doubles (e:es) = (if null xs2 then id else bothWays) e : doubles zs
 where 
   (xs1, ys) = partition same es
   (xs2, zs) = partition (same . flipE) ys
   same t   = from e == from t && to e == to t
   flipE e   = e {from = to e, to = from e}

instance Show a => Show (Graph a) where
   show g = unlines $ rec ("digraph " ++ name) g
    where 
      name   = M.findWithDefault "G" "label" (gProps g)
      indent = map ("   "++)
      rec graphtype g =
         [graphtype ++ " {"] ++
         indent [ k++"="++a++";" | (k, a) <- M.toList (gProps g) ] ++
         indent (map show (nodes g)) ++
         indent (map show (edges g)) ++
         indent (concat$ zipWith (\i s -> rec ("subgraph cluster" ++ show i) s) [0..] (subgraphs g)) ++
         ["}"]

instance Show a => Show (Node a) where
   show n = show (nValue n) ++ " [" ++ f props ++ "];"
    where
      f xs  = intercalate "," [ a ++ "=" ++ b | (a, b) <- xs ]
      props = M.toList (nProps n)
              
instance Show a => Show (Edge a) where
   show e = show (from e) ++ " -> " ++ show (to e) ++ " [" ++ f props ++ "];"
    where
      f xs  = intercalate "," [ a ++ "=" ++ b | (a, b) <- xs ]
      props = M.toList (eProps e)