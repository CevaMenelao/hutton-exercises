module Ejercicios_cap10 where

import Data.Char

--10_4:
act2 :: IO (Char,Char)
act2 = do   x <- getChar 
            y <- getChar
            return (x,y)

--10_5:
getLine2 :: IO String
getLine2 = do x <- getChar
              if x == '\n' then 
                  return [] 
              else do xs <- getLine2
                      return (x:xs)

putStr2 :: String -> IO ()
putStr2 [] = return ()
putStr2 (x:xs) = do putChar x
                    putStr2 xs

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " chacters"

--10_6:
sgetLine :: IO String 
sgetLine = do x <- getChar
              if x == '\n' then 
                 do putChar x 
                    return [] 
              else 
                  do  putChar '-'
                      xs <- sgetLine
                      return (x:xs)

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]

play :: String -> IO ()
play word = do putStr "? "
               guess <- getLine
               if guess == word then
                  putStrLn "You got it!"
               else
                   do putStrLn (match word guess)
                      play word

hangman :: IO ()
hangman = do putStrLn "Think of a word:"
             word <- sgetLine
             putStrLn "Try to guess it:"
             play word

--10_7:
next :: Int -> Int
next 1 = 2
next 2 = 1
next n = error "next out of range"

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
    where update r n = if r == row then n-num else n

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* " ))

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a 
                          putRow 2 b
                          putRow 3 c 
                          putRow 4 d 
                          putRow 5 e


newline :: IO ()
newline = putChar '\n'

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getLine
                     x' = head x
                     newline
                     if isDigit x' then
                         return (digitToInt x')
                     else 
                         do putStrLn "ERROR: Invalid digit"
                            getDigit prompt

play2 :: Board -> Int -> IO ()
play2 board player = 
    do  newline
        putBoard board
        if finished board then
            do newline
               putStr "Player "
               putStr (show (next player))
               putStrLn " wins!!!"
        else
            do newline
               putStr "Player "
               putStrLn (show player)
               row <- getDigit "Enter a row number: "
               num <- getDigit "Stars to remove: "
               if valid board row num then
                    play2 (move board row num) (next player)
               else
                    do newline
                       putStrLn "ERROR: Invalid move"
                       play2 board player


nim :: IO ()
nim = play2 initial 1


--10_8:
