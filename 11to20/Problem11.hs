data ResultList = Multiple Int Char | Single Char
  deriving (Show)

encode :: String -> [(Int, Char)]
encode ""  = []
encode str = [(length subString, head str)] ++ (encode (dropWhile (== head str) str))
  where subString = takeWhile (== head str) str


encodeModified :: String -> [ResultList]
encodeModified str = encodeModified' (encode str)

encodeModified' :: [(Int, Char)] -> [ResultList]
encodeModified' []                    = []

encodeModified' (x:[]) | (fst x) == 1 = [(Single (snd x))]
                       | otherwise    = [(Multiple (fst x) (snd x))]
 
encodeModified' (x:xs) | (fst x) == 1 = (Single (snd x)) : encodeModified' xs
                       | otherwise    = (Multiple (fst x) (snd x)) : encodeModified' xs

