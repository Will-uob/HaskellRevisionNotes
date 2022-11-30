import System.Random

main = do
  gen <- getStdGen
  putStrLn $ take 20 (randomRs ('a', 'z') gen)

-- Why don't we just use getStdGen twice? It's because we'd be using the same
-- global generator twice, which would yield an incorrect result.
main2 = do     
    gen <- getStdGen     
    putStrLn $ take 20 (randomRs ('a','z') gen)     
    gen' <- newStdGen  
    putStr $ take 20 (randomRs ('a','z') gen') 
