import Control.Monad.Writer

fib4 :: Integer -> Writer [Integer] Integer
fib4 n | n <  0 = error ("invalid input " ++ show n)
       | n == 0 = pure 0
       | n == 1 = pure 1
       | n >= 2 = do
                    tell [n]
                    x <- fib4 (n-2)
                    y <- fib4 (n-1)
                    pure (x+y)

result :: Integer -> [Integer]
result n = snd $ runWriter (fib4 n)
