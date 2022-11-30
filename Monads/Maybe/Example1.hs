f :: Int -> Maybe Int
f 0 = Nothing
f x = Just x

g :: Int -> Maybe Int
g 100 = Nothing
g x   = Just x

h :: Int -> Maybe Int
h x = case f x of
        Just n -> g n
        Nothing -> Nothing

h' :: Int -> Maybe Int
h' x = do n <- fx
          g n
