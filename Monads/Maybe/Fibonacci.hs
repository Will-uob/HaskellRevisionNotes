fib1 :: Integer -> Maybe Integer
fib1 n | n <  0  = Nothing
       | n == 0  = Just 0
       | n == 1  = Just 1
       | n >= 2  = case fib1 (n-2) of
                     Nothing -> Nothing
                     Just x  -> case fib1 (n-1) of
                                  Nothing -> Nothing
                                  Just y  -> Just (x+y)

fib1' :: Integer -> Maybe Integer
fib1' n | n <  0 = Nothing
        | n == 0 = pure 0
        | n == 1 = pure 1
        | n >= 2 = do
                     x <- fib1' (n-2)
                     y <- fib1' (n-1)
                     pure (x+y)

