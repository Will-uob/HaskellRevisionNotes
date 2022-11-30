factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

coprime :: Int -> Int -> Bool 
coprime x y = if [(a,b) | (a,b) <- zip (factors x) (factors y), a == b] == [(1,1)] then True else False
