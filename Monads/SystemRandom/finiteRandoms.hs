import System.Random

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)  
finiteRandoms n gen =   
    let (value, newGen) = random gen  
        (restOfList, finalGen) = finiteRandoms (n-1) newGen  
    in  (value:restOfList, finalGen)  

-- what if we want a random value in a range? We use randomR for this purpose!
-- it has a type of 
-- randomR :: (RandomGen g, Random a) :: (a, a) -> g -> (a, g)
-- example: ghci> randomR (1,6) (mkStdGen 359353)
--              (6, 149.... 40692)

-- there's also randomRs, which produces a stream of random values within the specified ranges.
-- example: ghci> take 10 $ randomRs ('a', 'z') (mkStdGen 3) :: [Char]
