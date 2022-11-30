gray :: Int -> [[Char]]
gray 1 = ["0", "1"]
-- gray 2 = (map ('0':) (gray 1)) ++ (map ('1':) (reverse (gray 1))) 
gray n = (map ('0':) (gray (n-1))) ++ (map ('1':) (reverse (gray (n-1))))
