factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime n = if factors n == [1, n] then True else False

primes :: Int -> [Int]
primes n = [x | x <- [2..n], isPrime x]

primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n primeFacts
  where primeFacts = [z | z <- (factors n), isPrime z]

primeFactors' :: Int -> [Int] -> [Int]
primeFactors' 1 (x:xs) = []
primeFactors' n (x:xs) | n `mod` x == 0 = [x] ++ primeFactors' (n `div` x) (x:xs)
                       | otherwise = primeFactors' n xs

prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n = prime_factors_mult' (primeFactors n)

numTimesFound x xs = (length . filter (==x)) xs

prime_factors_mult' :: [Int] -> [(Int, Int)]
prime_factors_mult' []     = []
prime_factors_mult' (x:xs) = [(x, numTimesFound x (x:xs))] ++ prime_factors_mult' (filter (/=x) (x:xs))

totient :: Int -> [Int]
totient n = [((p-1) * p)^(m-1) | (p, m) <- prime_factors_mult n]

goldbach :: Int -> (Int, Int)
goldbach n = head [(x, y) | x <- (primes n), y <- (primes n), x+y == n]

goldbachList :: Int -> Int ->[(Int, Int)]
goldbachList i j = [goldbach z | z <- [i..j], z `mod` 2 == 0]
