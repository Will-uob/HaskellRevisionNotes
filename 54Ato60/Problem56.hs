data Tree a = Empty
          | Fork a (Tree a) (Tree a) deriving (Show, Read, Eq, Ord)

symmetric :: Eq a => Tree a -> Bool
symmetric (Empty) = True
symmetric (Fork x l r) = if (Fork x l r) == (Fork x r l) then True else False
