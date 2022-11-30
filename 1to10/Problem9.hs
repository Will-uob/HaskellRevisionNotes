pack :: [Char] -> [[Char]]
pack []     = []
-- pack (x:[]) = [[x]]
pack (x:xs) = (takeWhile (==x) (x:xs)) : pack (dropWhile (==x) xs)
