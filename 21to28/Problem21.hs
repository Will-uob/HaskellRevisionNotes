insertAt :: a -> [a] -> Int -> [a]
insertAt ins xs n = take (n-1) xs ++ [ins] ++ drop (n-1) xs
