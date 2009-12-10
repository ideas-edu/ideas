-- Practicum 1: Getallen
-- gemaakt door Marinus Oosters (in m'n eentje) studentnummer 3370798
-- getest met GHC

-- ---------------------


-- Opgave 1: fromDec

fromDec :: [Int] -> Int
fromDec []      = 0
fromDec (x:xs)  = x * 10 ^ (length xs) + (fromDec xs)

-- Opgave 2: toDec

toDec :: Int -> [Int]
toDec a | a < 10   = [a]
        | a >= 10  = (toDec (a `div` 10)) ++ [ a `mod` 10 ]
        
-- Opgave 3: fromBin

fromBin :: [Int] -> Int
fromBin []     = 0
fromBin (x:xs) 
      | x==0 || x==1 = x * 2 ^ (length xs) + (fromBin xs)
      | otherwise    = error "Binaire getallen hebben alleen nullen en enen, dommerd!"
      
-- Opgave 4: toBin

toBin :: Int -> [Int]
toBin a | a < 2  = [a]
        | a >= 2 = (toBin (a `div` 2)) ++ [ a `mod` 2 ]
        
        
-- hulpfuncties voor opgaves 5, 6, 7 en 8 --
baselist = ['0'..'9'] ++ ['a'..'z']    -- lijst met Char-representatie van 'getallen' 
                                       -- zodat baselist!!waarde = "waarde"

waarde :: Int -> Char -> Int     -- functie die de waarde teruggeeft van een Char uit baselist
waarde base c | c `elem` (take base baselist) = length (eerste (break (==c) baselist))
              | otherwise                     = error "Ongeldig getal."

eerste :: (a,b) -> a   
eerste (a, _)  = a
                 


-- Opgave 5: fromBase

fromBase :: Int -> [Char] -> Int
fromBase _ []      = 0
fromBase a (x:xs) 
           | a<2 || a>36 = error "Alleen basen tussen 2 en 36 zijn mogelijk."
           | otherwise   = (waarde a x) * a ^ (length xs) + (fromBase a xs)

-- Opgave 6: toBase

toBase :: Int -> Int -> [Char]
toBase base num 
         | base<2 || base>36 = error "Alleen basen tussen 2 en 36 zijn mogelijk."
         | num<base   = [getal num]
         | num>=base  = (toBase base (num `div` base)) ++ [ getal (num `mod` base) ]
       where getal = (baselist!!)
       
-- Opgave 7: numbers
-- (ik maak wel gebruik van de functie uit opgave 5)

numbers :: Int -> [String] -> [(String,Int)]
numbers base list = zip flist [fromBase base num | num <- flist]
      where flist = filter (validstring base) list
            validstring base str = and [validchar base c | c <- str]
            validchar base ch = ch `elem` (take base baselist)
            

-- Opgave 8: grayCode
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode grondtal = (fromGray, toGray)
         where -- naar gray-code
               toGray :: Int -> [Char]
               toGray 0 = [head symbols] -- 0 is gewoon 0
               toGray n = trim [digits pw !! n | pw <- reverse [0..30]] --30 is het hoogste getal waarvoor
                                                                        --er nog iets gebeurt (daarna worden
                                                                        --de getallen te hoog voor Int)

               -- de nulletjes aan de voorkant eraf halen
               trim :: String -> String
               trim = dropWhile (==head symbols) 
               
               -- ik schaam me hier heel erg voor, maar mijn algoritme werkt alleen als grondtal
               -- even is. ik weet waarom, maar ik kreeg hem niet aan de gang en ik heb geen tijd
               -- meer.
               fromGrayOdd :: [Char] -> Int
               fromGrayOdd str = if and [ch `elem` symbols | ch <- str]
                                 then length (eerste (break (==trim str) values))
                                 else error "Ongeldig getal."
                         where values = [toGray n | n <- [0..]]
               
               -- dit is een soort van fromBase, maar hij houdt rekening met het feit dat in
               -- de gray-code de waarde van een getal soms (aantal waardes)-(normale waarde)
               -- is.
               fromGrayEven :: [Char] -> Int
               fromGrayEven str = if and [ch `elem` symbols | ch <- str]
                                 then fgh False str
                                 else error "Ongeldig getal."
                      where fgh :: Bool -> String -> Int
                            fgh _ []        = 0
                            fgh flip (x:xs) = thisone * grondtal ^ exponent + (fgh flipNext xs)  
                                 where exponent = length xs 
                                       thisone = val x flip
                                       flipNext = not $ even thisone
                                                  
                                                  
                            val :: Char -> Bool -> Int
                            val ch flip = indexOf ch ((if flip then reverse else id) symbols)
                                          
                            indexOf :: Char -> [Char] -> Int
                            indexOf el list | el `elem` list = length (eerste (break (==el) list))
                                            | otherwise      = -1
                  
               -- zo mogelijk het efficiente algoritme gebruiken   
               fromGray | even grondtal = fromGrayEven
                        | otherwise     = fromGrayOdd
               
               symbols = take grondtal baselist
               
               -- de getallen uit een kolom
               -- bijv 012: map digits [0..2] -> [2,1,0]
               digits :: Int -> [Char]
               digits n = if (symbolList == [])        -- dit gebeurt wanneer (length symbols)^n > 2^31
                          then cycle [head symbols]    -- het getal is dan toch te groot, dus cycle "0"
                                                       -- take pakt toch niets hoger dan 2^31 zelfs al
                                                       -- geef je hem een Integer ipv een Int
                          else cycle (symbolList ++ reverse symbolList) -- [0,1,1,0,0,1,1,0,...]
                      where symbolList = concat [take ((length symbols)^n) (cycle [a]) | a<-symbols]


-- Opgave 9: lookAndSay
lookAndSay :: Int -> [String]
lookAndSay n = lookAndSayLijst
     where -- lijst van stringrepresentaties van getallen, waarbij het volgende element
           -- steeds het vorige beschrijft
           lookAndSayLijst = (show n) : map beschrijving lookAndSayLijst
           
           -- de beschrijving van een string (bijv "1211" -> "111221")
           beschrijving :: String -> String
           beschrijving s = concat (map lookAndSayOne (split s))
                  where -- de beschrijving van een string die maar uit een soort char bestaat
                        -- (bijv "111" -> "31")
                        lookAndSayOne :: String -> String
                        lookAndSayOne l = (show (length l)) ++ [(head l)]
                        
                        -- een string in stukjes hakken zodat alle stukjes maar een soort char
                        -- hebben (bijv "1211" -> ["1", "2", "11"])
                        split :: String -> [String]
                        split [] = []
                        split x  = [same x] ++ split (drop (length $ same x) x)
                              where same l = takeWhile (==head l) l
               
                    
-- Opgave 10: keithGetallen
-- dit kan volgens mij beter -- ik weet alleen niet hoe
keithGetallen :: [Int]
keithGetallen = filter isKeith [1..] -- alle getallen die keith-getallen zijn
       where -- kijken of een getal in de keith-reeks zit
             isKeith :: Int -> Bool
             isKeith n | n<=9      = False -- spreekt voor zich
                        -- we hoeven natuurlijk nooit verder dan n te kijken: als we hem voorbij
                        -- zijn, zit hij er niet in. 
                        -- anders, als n niet in de lijst zit, zou hij oneindig ver zoeken.
                       | otherwise = n `elem` takeWhile (<=n) (keithReeks n)
                    where -- oneindige keith reeks
                          keithReeks :: Int -> [Int]
                          keithReeks n = begin ++ map keithGetal [1..]
                              where begin = (toDec n)
                                    keithGetal = last.(eindigeKeithReeks (length begin) begin)

                          -- maakt een aantal getallen van de keith reeks van begin
                          -- lengte: lengte begingetal -- begin: representatie begingetal
                          -- aantal: aantal getallen
                          eindigeKeithReeks :: Int -> [Int] -> Int -> [Int]
                          eindigeKeithReeks lengte begin aantal
                                | aantal == 0   = begin
                                | otherwise     = eindigeKeithReeks lengte (begin++[som]) (aantal-1)          
                              where som = sum (drop ((length begin)-lengte) begin)
            
            
                  