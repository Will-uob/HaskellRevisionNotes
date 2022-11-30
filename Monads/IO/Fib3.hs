fib3 :: Integer -> IO Integer
fib3 n | n <  0 = error ("invalid input " ++ show n)
       | n == 0 = pure 0
       | n == 1 = pure 1
       | n >= 2 = do
                    putStrLn ("call with n = " ++ show n)
                    x <- fib3 (n-2)
                    y <- fib3 (n-1)
                    pure (x+y)

