module Main (main) where

import Control.Monad
import Data.List
import Model
import Scanner
import Options
import qualified Data.Map as M
import System.Directory
import System.Environment
import Text.JSON

main :: IO ()
main = do
   flags <- getFlags
   forM_ (inputs flags) $ \path -> do
      isf <- doesFileExist path
      sys <- if isf 
             then readJSON path
             else scanSystem path
      unless (null $ systemModules sys) $ do
         forM_ (outputs flags) $
            report flags sys
         when (Save `elem` flags) $
            writeFile "out.json" (compactJSON $ toJSON sys)
    `catch`
      \_ -> return ()

readJSON :: FilePath -> IO System
readJSON path =
   readFile path >>= either fail fromJSON . parseJSON

report :: [Flag] -> System -> Output -> IO ()
report flags sys output = 
   case output of
   
      SystemSize ->
         putStrLn $ showTable id (["System size"], 
            [ ("system",  name sys)
            , ("modules", show (length ms))
            , ("code size", show (size sys))
            , ("full size", show (overallSize sys))
            ])
            
      PackageSize ->
         putStrLn $ showTable show $ (["Package size (LOC)"], 
            [ (s, n)
            | let spaces n = replicate n ' '
                  collect i a = concat 
                     [ (spaces (i*2) ++ s, linesOfCode b) : collect (i+1) b 
                     | (s, b) <- folders a 
                     ]
            , (s, n) <- collect 0 (systemHierarchy sys)
            ])
      
      ModuleSize ->
         putStrLn $ showTable show $ limit $ addStats $ sortTable (["Module size (LOC)"], 
            [ (name m, linesOfCode m)
            | m <- ms
            ])
            
      FunctionSize ->      
         putStrLn $ showTable show $ limit $ addStats $ sortTable (["Function size (LOC)"], 
            [ (name f ++ "  (" ++ name m ++ ")", linesOfCode f)
            | m <- ms, f <- moduleFunctions m
            ])
      
      Imports ->
         putStrLn $ showTable show $ limit $ addStats $ sortTable (["Imports in module"], 
            [ (name m, length (moduleImports m))
            | m <- ms
            ])
            
      ImportsOf -> do
         let table = foldr (\a -> M.insertWith (+) a 1) M.empty 
                   $ filter (`elem` map qname ms)
                   $ concatMap moduleImports ms
         putStrLn $ showTable show $ limit $ addStats $ sortTable (["Imports of module"], 
            [ (show a, n)
            | (a, n) <- M.toList table
            ])
 where
   ms = systemModules sys
   limit = case [ n | Limit n <- flags ] of  
              [] -> id
              ns -> limitTable (maximum ns)
   addStats
      | showStatistics flags = addAvgInt
      | otherwise = id

   {-
   writeFile "output/locs-data.js" $ 
      "var moduleLOCS =" ++ show (moduleLOCS sys) ++ ";"
      
   writeFile "output/imports-data.js" $ 
      let (labs, dat) = modImports sys
      in "var labs =" ++ show labs ++ ";\n" ++
         "var data = " ++ show dat ++ ";"

   writeFile "output/model.json" (compactJSON $ toJSON sys)

   --dot -Tsvg -O *.dot
   let rec pr h = 
          let f (s, a) = rec (pr++[s]) a
          in (pr, h) : concatMap f (folders h)
   forM_ (rec [] (systemHierarchy sys)) $ \(pr, h) -> do
      print (importsFile pr)
      writeFile ("imports/" ++ importsFile pr) (impGraph pr h sys)     
      
   let is = sort (map (fromIntegral . linesOfCode) ms)
       (q1, q2, q3) = quartiles is
       iqr = q3-q1
       lf  = q1-1.5*iqr
       uf  = q3+1.5*iqr
   putStrLn $ "Quartiles: " ++ show (q1, q2, q3) 
   putStrLn $ "Fences: " ++ show (lf, uf) -}
 
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
   
ex1 = quartiles $ sort [ 6, 47, 49, 15, 42, 41, 7, 39, 43, 40, 36]
ex2 = quartiles [ 7, 15, 36, 39, 40, 41] 