repli :: String -> Int -> String
repli ""     n = ""
repli (x:xs) n = (replicate n x) ++ repli xs n
