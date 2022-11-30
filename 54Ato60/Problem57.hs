data Tree a = Empty
          | Fork a (Tree a) (Tree a) deriving (Show, Read, Eq, Ord)

symmetric :: Eq a => Tree a -> Bool
symmetric (Empty) = True
symmetric (Fork x l r) = if (Fork x l r) == (Fork x r l) then True else False

construct :: [Int] -> Tree Int
construct xs = foldr (insertInto) Empty xs

insertInto :: Int -> Tree Int -> Tree Int
insertInto val Empty        = (Fork val (Empty) (Empty))
insertInto val (Fork x l r) | x > val   = Fork x l (insertInto val r)
                            | x < val   = Fork x (insertInto val l) r
                            | otherwise = error "Duplicate node!"

{-
add :: Ord a => a -> Tree a -> Tree a
add x Empty            = Branch x Empty Empty
add x t@(Branch y l r) = case compare x y of
                            LT -> Branch y (add x l) r
                            GT -> Branch y l (add x r)
                            EQ -> t

construct xs = foldl (flip add) Empty xs
-}

