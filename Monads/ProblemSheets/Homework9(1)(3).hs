-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE FlexibleContexts #-}

module Homework9 (applyfuns, updateNodes, eval, run) where

import Types

import Control.Monad.Except
import Control.Monad.State

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

-- Question 1
applyfuns :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d
applyfuns f1 f2 (Leaf n)     =  (Leaf (f2 n))
applyfuns f1 f2 (Fork l x r) = (Fork (applyfuns f1 f2 l) (f1 x) (applyfuns f1 f2 r)) 

-- Question 2
updateNodes :: Route -> (a -> a) -> BinTree a -> BinTree a
updateNodes []     f1  Empty         =  Empty
updateNodes (y:_)  f1  Empty         =  Empty
updateNodes []     f1 (Node l x r)   = (Node l (f1 x) r)
updateNodes (y:[]) f1 (Node l x r)   = if (y == GoLeft) then (Node (updateNodes [] f1 l) (f1 x) r) else (Node l (f1 x) (updateNodes [] f1 r))
updateNodes (y:ys) f1 (Node l x r)   | y == GoLeft  = (Node (updateNodes ys f1 l) (f1 x) r)
                                     | otherwise    = (Node l (f1 x) (updateNodes ys f1 r))

-- Question 3
eval :: MonadError String m => CalcExpr -> m Int
eval (Val a)    = return (a)
eval (Add a b)  = do
                    x <- eval a
                    y <- eval b
                    return (x+y)
eval (Sub a b)  = do
                    x <- eval a
                    y <- eval b
                    return (x-y)
eval (Mult a b) = do
                    x <- eval a
                    y <- eval b
                    return (x*y)
eval (Div a b)  = do 
                    x <- eval a
                    y <- eval b
                    when (y == 0) (throwError "Divide by zero!")
                    return (x `div` y) 

run :: (MonadState Int m, MonadError String m) => CalcCmd -> m ()
run EnterC         = return ()
run (StoreC a com) = do
                       x <- get
                       modify (\n -> (n - n) + a) 
                       y <- run com
                       return y

run (AddC a com)   = do
                       x <- get
                       modify (\n -> n + a)
                       y <- run com
                       return y

run (MultC a com)  = do
                       x <- get
                       modify (\n -> n * a)
                       y <- run com
                       return y 

run (DivC a com)   = do
                       x <- get
                       if (a == 0) 
                          then (throwError "Divide by zero!")
                          else do 
                                 modify (\n -> n `div` a)
                                 y <- run com
                                 return y

run (SubC a com)   = do
                       x <- get
                       modify (\n -> n - a)
                       y <- run com
                       return y 
