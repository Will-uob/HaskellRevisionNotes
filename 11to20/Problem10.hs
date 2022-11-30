{- Version that works for output of 9 
encode :: Eq a => [[a]] -> [(Int, a)]
encode []  = []
encode xss = [(length xs, head xs) | xs <- xss]
-}

encode :: String -> [(Int, Char)]
encode ""  = []
encode str = [(length subString, head str)] ++ (encode (dropWhile (== head str) str))
  where subString = takeWhile (== head str) str
