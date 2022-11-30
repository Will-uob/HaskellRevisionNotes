import System.Random

diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = do
  gen <- newStdGen
  let values = take n $ randomRs (1, m) gen
  return values
