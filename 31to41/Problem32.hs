myGCD :: Int -> Int -> Int
myGCD x y | x < 0 && y < 0 = myGCD (-x) (-y)
          | x < 0 && y > 0 = myGCD (-x) (y)
          | x > 0 && y < 0 = myGCD (-x) (y)
          | x == y    = x
          | x > y     = myGCD y (x-y)
          | otherwise = myGCD x (y-x)
