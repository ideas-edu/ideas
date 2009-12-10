fromBase' :: Int -> [Int] -> Int
fromBase' _ []     = 0
fromBase' n (x:xs) = x * n ^ length xs + fromBase' n xs

fromBin :: [Int] -> Int
fromBin = fromBase' 2
