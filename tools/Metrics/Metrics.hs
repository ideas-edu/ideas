module Main (main) where

import Control.Monad
import Data.List
import Data.Maybe
import Model
import Scanner
import Options
import qualified Data.Map as M
import System.Directory
import Text.JSON
import Text.HTML

main :: IO ()
main = do
   (flags, inputs) <- readOptions
   forM_ inputs $ \path -> do
      isf <- doesFileExist path
      sys <- if isf 
             then readJSON path
             else scanSystem path
      unless (null $ systemModules sys) $ do
         let ds = map (flagsData flags . flip outputData sys) (outputs flags)
         forM_ (modes flags) $ \mode -> 
            case mode of 
               Report -> report ds
               HTML   -> html   ds
         forM_ (exports flags) $ 
            export sys
         when (Save `elem` flags) $
            writeFile "out.json" (compactJSON $ toJSON sys)
    `catch`
      \_ -> return ()

readJSON :: FilePath -> IO System
readJSON path =
   readFile path >>= either fail fromJSON . parseJSON

data Data = TableString (Table String)
          | TableInt    (Table Int)
          | Hierarchy String (Hierarchy Int)

outputData :: Output -> System -> Data
outputData output =
   case output of
      SystemSize   -> TableString . systemSizeTable
      PackageSize  -> packageSizeData
      ModuleSize   -> TableInt . moduleSizeTable
      FunctionSize -> TableInt . functionSizeTable
      Imports      -> TableInt . importsTable
      ImportsOf    -> TableInt . importsOfTable

flagsData :: [Flag] -> Data -> Data
flagsData flags = f (limit . addStats)
 where
   f g (TableInt a) = TableInt (g a)
   f _ item         = item
 
   limit = 
      case [ n | Limit n <- flags ] of  
         [] -> id
         ns -> limitTable (maximum ns)
         
   addStats
      | showStatistics flags = addAvgInt
      | otherwise = id

report :: [Data] -> IO ()
report ds = 
   forM_ ds $ \item -> putStrLn $
   case item of
      TableInt a    -> showTable show a
      TableString a -> showTable id a
      Hierarchy s a -> showTable show (hierarchyTable sum s a)

html :: [Data] -> IO ()
html ds = 
   writeFile "out.html" $ show $ htmlPage "title" (Just "ideas.css") $  
   divClass "content" $ do
   forM_ ds $ \item -> 
      case item of
         TableInt a    -> tableToHTML show a
         TableString a -> tableToHTML id a
         Hierarchy s a -> tableToHTML show (hierarchyTable sum s a)

tableToHTML :: (a -> String) -> Table a -> HTMLBuilder
tableToHTML f (title, xs) = do
   preText (unlines title)
   table False $ map (\(a, b) -> [text a, text (f b)]) xs 

systemSizeTable :: System -> Table String
systemSizeTable sys = 
   (["System size"], 
            [ ("system",  name sys)
            , ("modules", show (length (systemModules sys)))
            , ("code size", show (size sys))
            , ("full size", show (overallSize sys))
            ])

packageSizeData :: System -> Data
packageSizeData = 
   Hierarchy "Package size (LOC)" . fmap linesOfCode . systemHierarchy

moduleSizeTable :: System -> Table Int
moduleSizeTable sys =
         sortTable (["Module size (LOC)"], 
            [ (name m, linesOfCode m)
            | m <- systemModules sys
            ])
            
functionSizeTable :: System -> Table Int
functionSizeTable sys = 
         sortTable (["Function size (LOC)"], 
            [ (name f ++ "  (" ++ name m ++ ")", linesOfCode f)
            | m <- systemModules sys, f <- moduleFunctions m
            ])

importsTable :: System -> Table Int
importsTable sys = sortTable (["Imports in module"], 
            [ (name m, length (moduleImports m))
            | m <- systemModules sys
            ])

importsOfTable :: System -> Table Int
importsOfTable sys = sortTable (["Imports of module"], 
            [ (show a, n)
            | (a, n) <- M.toList table
            ])
 where
   table = foldr (\a -> M.insertWith (+) a 1) M.empty 
                   $ filter (`elem` map qname (systemModules sys))
                   $ concatMap moduleImports (systemModules sys)

hierarchyTable :: ([a] -> a) -> String -> Hierarchy a -> Table a
hierarchyTable f title h = ([title], rec 0 h)
 where
   rec i a = concat 
      [ (spaces (i*2) ++ s, f (toList b)) : rec (i+1) b 
      | (s, b) <- folders a 
      ]
   spaces n = replicate n ' '

export :: System -> Export -> IO ()
export sys export =
   case export of
      
      ExportLOC mf -> do
         writeFile (fromMaybe "loc.js" mf) $ 
            "var metrics = {};\nmetrics.loc =" ++ show (moduleLOCS sys) ++ ";"
            
      ExportImports mf -> do
         writeFile (fromMaybe "imports.js" mf) $ 
            let (labs, dat) = modImports sys
            in "var labs =" ++ show labs ++ ";\n" ++
               "var data = " ++ show dat ++ ";"
               
      ImportGraph mf -> do --dot -Tsvg -O *.dot
         let rec pr h = 
                let f (s, a) = rec (pr++[s]) a
                in (pr, h) : concatMap f (folders h)
         forM_ (rec [] (systemHierarchy sys)) $ \(pr, h) -> do
            print (importsFile pr)
            writeFile (maybe "" (++"/") mf ++ importsFile pr) $
               impGraph pr h sys
 
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
showTable f (title, xs) 
   | null xs   = []
   | otherwise = unlines (header ++ body)
 where
   header = title ++ [line]
   body   = map pp xs
   line   = replicate len '-'
   pp (a, b) = alignl n a ++ " : " ++ f b
   n   = maximum (0 : map (length . fst) xs)
   len = maximum (map length (title++body))

addAvgInt :: Table Int -> Table Int
addAvgInt (title, xs) = (title++[stats1, stats2], xs)
 where
   (a, sd) = avgStdev (map snd xs)
   (q1, q2, q3) = quartiles (sort $ map (fromIntegral . snd) xs)
   stats1  = "   (avg=" ++ showDouble 2 a ++ ", stdev=" ++ showDouble 2 sd ++ ")"
   stats2  = "   (Q1=" ++ showDouble 2 q1 ++ ", Q2=" ++ showDouble 2 q2 ++ ", Q3=" ++ showDouble 2 q3 ++ ")"

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

{-
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
weighted = map (\(a, b) -> (a, a*b)) -}

alignr :: Int -> String -> String
alignr n s = replicate (n - length s) ' ' ++ s

alignl :: Int -> String -> String
alignl n s = take n (s ++ repeat ' ')

importsFile :: [String] -> String
importsFile prefix = intercalate "-" ("imports":prefix) ++ ".dot"

impGraph :: [String] -> Hierarchy Module -> System -> String 
impGraph prefix hier sys = show $ href (importsFile [] ++ ".svg") $ 
   sub `addTo` (graph (name sys) nodes3 (doubles edges2)) 
 where
   allms = systemModules sys 
   
   sub = (filled $ color "lightgrey" $ href "" $ graph (intercalate "." prefix) (nodes1 ++ nodes2) edges1) 
   nodes1 = [ filled $ color "white" $ label (unqualified (qname n)) $ node (name n)
            | n <- localItems hier
            ]
   nodes2 = [ filled $ color "yellow" $ shape "rectangle" $ 
              tooltip sn $ label sn $ href (importsFile (prefix++[n]) ++ ".svg") $
              node (intercalate "." (prefix++[n]) ++ "+I")
            | (n, a) <- folders hier
            , let sn = show $ n ++ " (" ++ show (length (toList a)) ++ ")"
            ]

   nodes3 = [ if n `elem` map name allms 
              then node n
              else filled $ color "yellow" $ shape "rectangle" $
                   href (importsFile (qualifiers qn ++ [unqualified qn]) ++ ".svg") $
                   node n
            | n <- nub (map to edges2)
            , let qn = makeQName n
            ]
   
   (es1, es2) = partition (internal . to) es 
    where
      es = [ edge (qname m) i | m <- toList hier, i <- moduleImports m ]
      internal i = i `elem` map qname (toList hier)
      
   edges1 = doubles [ fmap f $ e
                    | e <- es1
                    ]
   edges2 = doubles [ dotted $ color "grey" $ fmap f $ e
                    | e <- es2
                    , to e `elem` map qname allms
                    ]
   f i = g (qualifiers i ++ [unqualified i]) prefix
       
   g (a:as) (b:bs) 
      | a == b    = a ++ "." ++ g as bs
      | otherwise = a
   g [a] [] = a
   g (a:_) [] = a ++ "+I"
   g _ _ = []

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

instance Functor Edge where
   fmap f (Edge x y ps) = Edge (f x) (f y) ps

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
doubles (e:es) 
   | from e == to e = doubles es
   | otherwise      = (if null xs2 then id else bothWays) e : doubles zs
 where 
   ys        = filter (not . same) es
   (xs2, zs) = partition (same . flipE) ys
   same t    = from e == from t && to e == to t
   flipE x   = x {from = to x, to = from x}

instance Show a => Show (Graph a) where
   show g0 = unlines $ rec ("digraph " ++ getName g0) g0
    where 
      getName = M.findWithDefault "G" "label" . gProps
      indent = map ("   "++)
      rec graphtype g =
         [graphtype ++ " {"] ++
         indent [ k++"="++a++";" | (k, a) <- M.toList (gProps g) ] ++
         indent (map show (nodes g)) ++
         indent (map show (edges g)) ++
         indent (concat$ zipWith (\i s -> rec ("subgraph cluster" ++ show i) s) [0::Int ..] (subgraphs g)) ++
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
      
compactJSON :: JSON -> String
compactJSON json = 
   case json of
      Array  xs -> 
         "[" ++ intercalate "," (map compactJSON xs) ++ "]"
      Object xs -> 
         let f (k, a) = show k ++ ":" ++ compactJSON a
         in "{" ++ intercalate "," (map f xs) ++ "}"
      _ -> show json
      
quartiles :: Fractional a => [a] -> (a, a, a)
quartiles list = (median xs, mid, median ys)
 where
   (xs, mid, ys) = splitMedian list

median :: Fractional a => [a] -> a
median = (\(_, m, _) -> m) . splitMedian

splitMedian :: Fractional a => [a] -> ([a], a, [a])
splitMedian list 
   | null list = error "splitMedian []"
   | even n    = (xs, (last xs+mid)/2, mid:ys)
   | otherwise = (xs++[mid], mid, mid:ys)
 where
   n = length list
   (xs, mid:ys) = splitAt (n `div` 2) list