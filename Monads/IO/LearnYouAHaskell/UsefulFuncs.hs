import Data.Char
import Control.Monad

-- putStr: like putStrLn without the newline.

main1 = do putStr "Hey, "  
           putStr "I'm "  
           putStrLn "Andy!" 

-- putChar:  takes a character and returns an I/O action that will print it out to the terminal.
-- note: putStr is defined recursively with the help of putChar.

main2 = do   putChar 't'  
             putChar 'e'  
             putChar 'h'

-- print takes a value of any type that's an instance of Show 
-- (meaning that we know how to represent it as a string), 
-- calls show with that value to stringify it and then outputs that string to the terminal. 
-- Basically, it's just putStrLn . show. It first runs show on a value and then feeds that to putStrLn, 
-- which returns an I/O action that will print out our value.

main3 = do   print True  
             print 2  
             print "haha"  
             print 3.2  
             print [3,4,3]  

-- getChar
main4 = do     
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            main4  
        else return () 

main5 = do  c <- getChar  
            when (c /= ' ') $ do  
               putChar c  
               main5

-- sequence: takes a list of I/O actions and returns an I/O actions that will perform those 
-- actions one after the other. The result contained in that I/O action will be a list of the 
-- results of all the I/O actions that were performed. Its type signature is 
main6 = do
  rs <- sequence [getLine, getLine, getLine]
  print rs

-- mapM and mapM_: mapM takes a function and a list, maps the function over the list and then
-- sequences it. mapM_ does the same, but throws away the result.

-- forever : takes an I/O action and returns an I/O action that just repeats the I/O action it got forever.
main7 = forever $ do
  putStr "Give me some input: "
  l <- getLine
  putStrLin $ map toUpper l

-- forM: the same as mapM, but with the parameters switched.

main8 = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors  
