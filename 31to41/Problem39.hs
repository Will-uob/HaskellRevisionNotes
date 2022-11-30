factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime n = if factors n == [1, n] then True else False

primesR :: Int -> Int -> [Int]
primesR i j = [x | x <- [i..j], isPrime x]
