dropEvery :: Eq a => [a] -> Int -> [a]
dropEvery [] n = []
dropEvery xs n = [snd x | x <- zip [1..] xs, fst x `mod` n /= 0]
