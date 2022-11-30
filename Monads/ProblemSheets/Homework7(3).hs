-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Homework7 (phoneToString , stringToPhone , fingerTaps) where

import Types

import Data.Char
import Data.List

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 7a -}
-- Star caplitalizes the next letter. If pressed twice, reverts to lower case.
changeBool :: Bool -> Bool
changeBool True = False
changeBool False = True

phoneToString :: [(Button, Presses)] -> Text
phoneToString xs = phoneToString' xs False 

capitalize :: Bool -> Char -> Char
capitalize starPressed digit | starPressed == True = toUpper(digit)
                             | otherwise = digit

phoneToString' :: [(Button, Presses)] -> Bool -> Text 
phoneToString' []                    isCap = []
phoneToString' ((button,presses):[]) isCap | button == '1' = "1"
                                           | button == '2' = [capitalize isCap (snd x) | x <- (zip [0..] ['a','b','c','2']), ((presses-1) `mod` 4) == fst x] -- ['a', 'b', 'c', '2'] (!!) (presses `mod` 5)
                                           | button == '3' = [capitalize isCap (snd x) | x <- (zip [0..] ['d','e','f','3']), ((presses-1) `mod` 4) == fst x] 
                                           | button == '4' = [capitalize isCap (snd x) | x <- (zip [0..] ['g','h','i','4']), ((presses-1) `mod` 4) == fst x] 
                                           | button == '5' = [capitalize isCap (snd x) | x <- (zip [0..] ['j','k','l','5']), ((presses-1) `mod` 4) == fst x] 
                                           | button == '6' = [capitalize isCap (snd x) | x <- (zip [0..] ['m','n','o','6']), ((presses-1) `mod` 4) == fst x] 
                                           | button == '7' = [capitalize isCap (snd x) | x <- (zip [0..] ['p','q','r','s', '7']), ((presses-1) `mod` 5) == fst x] 
                                           | button == '8' = [capitalize isCap (snd x) | x <- (zip [0..] ['t','u','v', '8']), ((presses-1) `mod` 4) == fst x] 
                                           | button == '9' = [capitalize isCap (snd x) | x <- (zip [0..] ['w','x','y','z','9']), ((presses-1) `mod` 5) == fst x] 
                                           | button == '0' = [capitalize isCap (snd x) | x <- (zip [0..] [' ','0']), ((presses-1) `mod` 2) == fst x] 
                                           | button == '#' = [capitalize isCap (snd x) | x <- (zip [0..] ['.',',']), ((presses-1) `mod` 2) == fst x] 
                                           | otherwise = "Error!"

phoneToString' ((button,presses):xs) isCap | button == '*' = if (presses `mod` 2 == 0) then phoneToString' xs isCap else phoneToString' xs (changeBool isCap)
                                           | otherwise = if isCap == False then phoneToString' ((button,presses):[]) isCap ++ phoneToString' xs isCap else
                                                                                phoneToString' ((button,presses):[]) isCap ++ phoneToString' xs (changeBool isCap)

{- Question 7b -}

maybeToint :: Maybe Int -> Int
maybeToint Nothing  = -1
maybeToint (Just a) = a

stringToPhone :: Text -> [(Button, Presses)]
stringToPhone xs = stringToPhone' xs

stringToPhone' :: Text -> [(Button, Presses)]
stringToPhone' []     = []

stringToPhone' (x:[]) | x `elem` ['1']                     = [('1', 1)] 
                      | x `elem` ['a', 'b', 'c', '2']      = [('2', (maybeToint (findIndex (==x) ['a','b','c','2'])+1))]
                      | x `elem` ['d', 'e', 'f', '3']      = [('3', (maybeToint (findIndex (==x) ['d','e','f','3'])+1))]
                      | x `elem` ['g', 'h', 'i', '4']      = [('4', (maybeToint (findIndex (==x) ['g','h','i','4'])+1))]
                      | x `elem` ['j', 'k', 'l', '5']      = [('5', (maybeToint (findIndex (==x) ['j','k','l','5'])+1))]
                      | x `elem` ['m', 'n', 'o', '6']      = [('6', (maybeToint (findIndex (==x) ['m','n','o','6'])+1))]
                      | x `elem` ['p', 'q', 'r', 's', '7'] = [('7', (maybeToint (findIndex (==x) ['p','q','r','s','7'])+1))]
                      | x `elem` ['t', 'u', 'v', '8']      = [('8', (maybeToint (findIndex (==x) ['t','u','v','8'])+1))]
                      | x `elem` ['w', 'x', 'y', 'z', '9'] = [('9', (maybeToint (findIndex (==x) ['w','x','y','z','9'])+1))]
                      | x `elem` [' ', '0']                = [('0', (maybeToint (findIndex (==x) [' ', '0'])+1))]
                      | x `elem` ['.', ',']                = [('#', (maybeToint (findIndex (==x) ['.', ','])+1))]
                      | otherwise = [('0', 0)]

stringToPhone' (x:xs) = if isUpper(x) then [('*', 1)] ++ (stringToPhone' ((toLower x):[])) ++ (stringToPhone' xs)
                                      else (stringToPhone' (x:[])) ++ (stringToPhone' xs)
 
{- Question 7c -}
fingerTaps :: Text -> Presses
fingerTaps inputText = sum [b | (a,b) <- (stringToPhone' inputText)] 
