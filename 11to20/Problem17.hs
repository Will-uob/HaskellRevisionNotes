split :: String -> Int -> (String, String)
split str n | (length str) < n = error "String is too short"
            | otherwise = (take n str, drop n str)
