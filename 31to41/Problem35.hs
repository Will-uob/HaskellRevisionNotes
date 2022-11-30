factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime n = if factors n == [1, n] then True else False

primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n primeFacts
  where primeFacts = [z | z <- (factors n), isPrime z]

primeFactors' :: Int -> [Int] -> [Int]
primeFactors' 1 (x:xs) = []
primeFactors' n (x:xs) | n `mod` x == 0 = [x] ++ primeFactors' (n `div` x) (x:xs)
                       | otherwise = primeFactors' n xs
