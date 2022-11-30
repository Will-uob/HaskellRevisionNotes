data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem n)  = [n]
flatten (List xs) = concat (map (flatten) xs)
