
main = do
  -- we could also have written "foo <- putStrLn "Hello, what's your name?""
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn ("Hey " ++ name ++ ", you rock!")
