module Ejercicio_cap11_4 where

--11_2:
import Data.Char
import Data.List
import System.IO
import System.Random hiding (next)

size :: Int
size = 3

data Player = O | B | X 
              deriving (Eq, Ord, Show)

type Grid = [[Player]]

next :: Player -> Player
next O = X
next B = B
next X = O


--11_3:
empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
    where
        os = length (filter (== O) ps)
        xs = length (filter (== X) ps)
        ps = concat g

wins ::Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
    where  
        line = all (==p)
        rows = g 
        cols = transpose g
        dias = [diag g, diag (map reverse g)]

--transpose [[1,2,3],[4,5,6],[7,8,9]]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g


--11_4:
putGrid :: Grid -> IO ()
putGrid =
    putStrLn . unlines . concat . interleave bar . map showRow
    where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer 
          where 
              beside    = foldr1 (zipWith (++))
              bar       = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]
-- showPlayer p = ["   ", " "++ show p ++ " ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys


--11_5:
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move:: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then [chop size (xs ++ [p] ++ ys)] else []
    where (xs,B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)


--11_6:
getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then 
                       return (read xs)
                    else 
                        do putStrLn "ERROR: Invalid number"
                           getNat prompt
        

--11_7:
tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do    cls
                goto (1,1)
                putGrid g
                run' g p

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int,Int) -> IO ()
goto (x,y) = putStr ("\ESC["++ show y ++ ";" ++ show x ++ "H")

run' :: Grid -> Player -> IO ()
run' g p | wins O g  = putStrLn "Player O wins ! \n"
         | wins X g  = putStrLn "Player X wins ! \n"
         | full g    = putStrLn "It's a draw! \n"
         | otherwise = do i <- getNat (prompt p)
                          case move g i p of 
                           [] -> do putStrLn "ERROR: Invalid move: "
                                    run' g p 
                           [g'] -> run g' (next p)  

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "


--11_8:
data Tree a = Node a [Tree a]
              deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p | won g       = []
          | full g      = []
          | otherwise   = concat [move g i p | i <- [0..((size^2)-1)]]

--11_9:
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9


--11_10:
minimax :: Tree Grid -> Tree (Grid,Player)
minimax (Node g []) | wins O g  = Node (g,O) []
                    | wins X g  = Node (g,X) []
                    | otherwise = Node (g,B) []
minimax (Node g ts) | turn g == O = Node (g, minimum ps) ts'
                    | turn g == X = Node (g, maximum ps) ts'
                                    where
                                        ts' = map minimax ts
                                        ps  = [p | Node (_,p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
               where 
                   tree = prune depth (gametree g p)
                   Node (_,best) ts = minimax tree 


--11_11:


main :: IO ()
main = do hSetBuffering stdout NoBuffering
          putStr "Do you want to change the lenght of a wining line? (y/n)  "
          es <- getLine
          let e = head es
          if length es == 1 then
              if e == 'y' || e == 'n' then
                  if e == 'y' then 
                      do ss <- getNat "What length do you want? "
                         let size = ss 
                         putStrLn ("Now for win, complete " ++ show ss ++ " lines to win")
                  else 
                      do let size = 3
                         putStrLn ("Remember complete 3 lines to win")
              else 
                  do putStrLn "ERROR: You need to put y or n"
                     main
          else 
              do putStrLn "ERROR: You must only put a letter"
                 main
          putStr "Do you want to play fst or snd? (1/2)  "
          xs <- getLine
          let x = head xs 
          if all isDigit xs then
              if x == '1' || x == '2' then
                  if x == '1' then 
                      play empty O
                  else 
                      play empty X
              else 
                  do putStrLn "ERROR: You need to put 1 or 2"
                     main
          else 
              do putStrLn "ERROR: You must to put a number"
                 main

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1,1)
              putGrid g    
              play' g p

play' :: Grid -> Player -> IO ()
play' g p | wins O g  = putStrLn "Player O wins ! \n"
          | wins X g  = putStrLn "Player X wins ! \n"
          | full g    = putStrLn "It's a draw! \n"
          | p == O    = do i <- getNat (prompt p)
                           case move g i p of 
                               [] -> do putStrLn "ERROR: Invalid move: "
                                        play' g p 
                               [g'] -> play g' (next p)  
          | p == X    = do putStr "Player X is thinking ... \n"

                           (play $! (bestmove g p)) (next p)
