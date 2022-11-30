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

-- Use of the IO() monad.
table :: (Bool -> Bool -> Bool) -> IO () -- Type of the table function.
table f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b) --We show the table.
                                | a <- [True, False], b <- [True, False]]


