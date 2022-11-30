import Control.Monad (replicateM)

-- Implementation of AND 
and' :: Bool -> Bool -> Bool
and' True True  = True
and' _    _     = False

-- Implementation of OR
or'  :: Bool -> Bool -> Bool
or' False False = False
or' _     _     = True

-- Implementation of NAND
nand :: Bool -> Bool -> Bool
nand a b = not $ and' a b

-- Implementation of NOR
nor  :: Bool -> Bool -> Bool
nor  a b = not $ or'  a b

-- Implementation of XOR
xor  :: Bool -> Bool -> Bool
xor True  True  = False
xor False False = False
xor _     _     = True

-- Implementation of implication function. Only false if True implies False
impl' a b = (not a) `or'` b

-- Equality function.
equ'  True  True  = True
equ'  False False = True
equ'  _     _     = False

-- functions as in solution 46
infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 3 `equ'` -- was 7, changing it to 3 got me the same results as in the original question :(

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn [toStr a ++ " => " ++ show (f a) | a <- args n]
    where args n = replicateM n [True, False]
          toStr = unwords . map (\x -> show x ++ space x)
          space True = "  "
          space False = " "


