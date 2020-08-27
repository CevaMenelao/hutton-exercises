module Ejercicios_cap10 where

import           Data.Char
import           System.IO
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
                     let x' = head x
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

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC["++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do   goto p
                    putStr xs

width :: Int
width = 10

height :: Int
height = 10

type Board2 = [Pos]

glider :: Board2
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

showcells :: Board2 -> IO ()
showcells b = sequence_ [writeat p "O" | p <- b]

isAlive :: Board2 -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board2 -> Pos -> Bool
isEmpty b p = not (isAlive b p)

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) +1,
              ((y-1) `mod` height) +1 )

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1),(x,y-1),
                          (x+1,y-1),(x-1,y),
                          (x+1,y),(x-1,y+1),
                          (x,y+1),(x+1,y+1)]

liveneighbs :: Board2 -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board2 -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

births :: Board2 -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3]

nextgen :: Board2 -> Board2
nextgen b = survivors b ++ births b

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

life :: Board2 -> IO ()
life b = do     cls
                showcells b
                wait 500000
                life (nextgen b)

-- 10_10_1:
--putStr2 :: String -> IO ()
--putStr2 [] = return ()
--putStr2 (x:xs) = do putChar x
--                    putStr2 xs
putStr3 :: String -> IO ()
putStr3 cs = sequence_ [putChar c | c <- cs ]




-- 10_10_2:
--putBoard :: Board -> IO ()
--putBoard [a,b,c,d,e] = do putRow 1 a
--                          putRow 2 b
--                          putRow 3 c
--                          putRow 4 d
--                          putRow 5 e
    
putBoard2 :: Board -> IO ()
putBoard2 = aux 1

aux :: Int -> Board -> IO ()
aux _ []        = return ()
aux n (x:xs)    = do putRow n x
                     aux (n+1) xs




-- 10_10_3:
putBoard3 :: Board -> IO ()
putBoard3 xs = sequence_ [putRow n x | (n,x) <- zip [1..(length xs)] xs ]




-- 10_10_4:
numsreader :: IO Int
numsreader = do xs <- getLine 
--              if all isDigit xs then                    Capitulo 11
                if and [isDigit x' | x' <- xs] then
                    return (foldl (\x y -> 10*x + y) 0 (map digitToInt xs))
                else 
                    do putStrLn "ERROR: Invalid number, try again"
                       numsreader

sumator :: Int -> IO Int
sumator 0 = return (0)
sumator n = do x1 <- numsreader
               x2 <- sumator (n-1)
               return (x1 + x2)

adder :: IO ()
adder = do putStr "How many numbers? "
           nums <- numsreader
           sol <- sumator nums
           putStrLn ("The total is " ++ show sol)




-- 10_10_5:
adder2 :: IO ()
adder2 = do putStr "How many numbers? "
            nums <- numsreader
            sol <- sequence [numsreader | _ <-[1..nums]]
            putStrLn ("The total is " ++ show (sum sol))



-- 10_10_6:
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

readLine :: IO String
readLine = do x <- getCh
              if x == '\n' then
                  do putChar x
                     return []
              else
                  if x == '\DEL' then
                      do putChar '\b'
                         xs <- readLine
                         return xs
                  else
                      do putChar x
                         xs <- readLine
                         return (x:xs)

