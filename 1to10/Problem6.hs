isPalindrome :: Eq a => [a] -> Bool
isPalindrome []     = False
isPalindrome xs     = (xs == reverse xs)
