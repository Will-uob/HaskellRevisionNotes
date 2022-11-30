import System.Random
import Control.Monad (replicateM)

-- Solution included in the problem sets.
rnd_select :: [a] -> Int -> IO [a] -- We are using the IO() monad
rnd_select [] _ = return [] -- If we cannot select any elements, we are fucked.
rnd_select l  n  
    | n<0 = error "N must be greater than zero." -- We cannot select a number of elements less than 1
    | otherwise = do pos <- replicateM n $  -- We perform the following action three times.
                               getStdRandom $ randomR (0, (length l)-1) -- Generate a random number.
                     return [l!!p | p <- pos] -- return a list of the random elements we got.

-- My own solution using randomRs.
-- Execute using `stack repl --package random Problem23.hs`
rnd_select :: [a] -> Int -> IO [a]
rnd_select [] n = return []
rnd_select xs n | n < 0 = error "What the fuck is this?"
                | otherwise = do
                           gen <- newStdGen
                           let positions = take 3 (randomRs (0, (length xs)-1) gen)
                           return [xs !! z | z <- positions]                                                         
