module Ejercicio_cap11_4 where

--11_2:
import Data.Char
import Data.List
import System.IO
import System.Random hiding (next)


data Player = O | B | X 
              deriving (Eq, Ord, Show)

type Grid = [[Player]]

next :: Player -> Player
next O = X
next B = B
next X = O


--11_3:
empty :: Int -> Grid
empty size = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
    where
        os = length (filter (== O) ps)
        xs = length (filter (== X) ps)
        ps = concat g

wins ::Player -> Grid -> Int -> Bool
wins p g size = any line (rows ++ cols ++ dias)
    where  
        line = all (==p)
        rows = g 
        cols = transpose g
        dias = [diag g size, diag (map reverse g) size]

--transpose [[1,2,3],[4,5,6],[7,8,9]]

diag :: Grid -> Int -> [Player]
diag g size = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Int -> Bool
won g size = wins O g size || wins X g size


--11_4:
putGrid :: Int -> Grid -> IO ()
putGrid size = putStrLn . unlines . concat . interleave bar . map showRow
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
valid :: Grid -> Int -> Int -> Bool
valid g i size = 0 <= i && i < size^2 && concat g !! i == B

move:: Grid -> Int -> Player -> Int ->[Grid]
move g i p size = if valid g i size then [chop size (xs ++ [p] ++ ys)] else []
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
tictactoe :: Int -> IO ()
tictactoe size = run (empty size) O size 

run :: Grid -> Player -> Int -> IO ()
run g p size = do    cls
                     goto (1,1)
                     putGrid size g
                     run' g p size

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int,Int) -> IO ()
goto (x,y) = putStr ("\ESC["++ show y ++ ";" ++ show x ++ "H")

run' :: Grid -> Player -> Int -> IO ()
run' g p size | wins O g size = putStrLn "Player O wins ! \n"
              | wins X g size = putStrLn "Player X wins ! \n"
              | full g        = putStrLn "It's a draw! \n"
              | otherwise     = do i <- getNat (prompt p)
                                   case move g i p size of
                                       [] -> do putStrLn "ERROR: Invalid move: "
                                                run' g p size
                                       [g'] -> run g' (next p) size

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "


--11_8:
data Tree a = Node a [Tree a]
              deriving Show

gametree :: Grid -> Player -> Int -> Tree Grid
gametree g p size = Node g [gametree g' (next p) size| g' <- moves g p size]

moves :: Grid -> Player -> Int -> [Grid]
moves g p size | won g size  = []
               | full g      = []
               | otherwise   = concat [move g i p size| i <- [0..((size^2)-1)]]

--11_9:
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

{-
pruneab :: Tree Grid -> Int -> Tree Grid
pruneab (Node g []) _ = Node g []
pruneab (Node g ts) size | wins O g  = Node (g,O) []
                         | wins X g  = Node (g,X) []
                         | otherwise = Node (g,B) []
pruneab (Node g ts) size | turn g == O = Node (g, minimum ps) ts'
                         | turn g == X = Node (g, maximum ps) ts'
                                    where
                                        ts' = map pruneab ts size
                                        ps  = [p | Node (_,p) _ <- ts']
-}

depth :: Int
depth = 9


--11_10:
minimax :: Int -> Tree Grid -> Tree (Grid,Player)
minimax size (Node g []) | wins O g size = Node (g,O) []
                         | wins X g size = Node (g,X) []
                         | otherwise = Node (g,B) []
minimax size (Node g ts) | turn g == O = Node (g, minimum ps) ts'
                         | turn g == X = Node (g, maximum ps) ts'
                                    where
                                        ts' = map (minimax size) ts
                                        ps  = [p | Node (_,p) _ <- ts']


bestmove :: Grid -> Player -> Int -> Grid
bestmove g p size = head [g' | Node (g',p') _ <- ts, p' == best]
               where 
                   tree = prune depth (gametree g p size)
                   Node (_,best) ts = minimax size tree 

bestmove' :: Tree Grid -> Player -> Int -> Grid
bestmove' t p size = head [g' | Node (g',p') _ <- ts, p' == best]
               where 
                   tree = prune depth t
                   Node (_,best) ts = minimax size tree
--11_11:

serchandremp :: Tree Grid -> Grid -> Tree Grid
serchandremp t g = head [Node g' sbts | Node g' sbts <- ts, g' == g ]
    where Node _ ts = t

changenl :: IO Int
changenl = do putStr "Do you want to change the lenght of a wining line? (y/n)  "
              es <- getLine
              let e = head es
              if length es == 1 then
                  if e == 'y' || e == 'n' then
                      if e == 'y' then
                          do ss2 <- getNat "What length do you want? "                                               -- Cambio
                             putStrLn ("Now for win, complete " ++ show ss2 ++ " lines to win")
                             return ss2 
                      else 
                          do putStrLn ("Remember complete 3 lines to win")
                             return 3
                  else 
                      do putStrLn "ERROR: You need to put y or n"
                         changenl
              else
                  do putStrLn "ERROR: You must only put a letter"
                     changenl

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          size <- changenl
          putStr "Do you want to play fst or snd? (1/2)  "                                                      -- Cambio 
          xs <- getLine
          let x = head xs 
          if all isDigit xs then
              if (x == '1' || x == '2') && length xs == 1 then
                  if x == '1' then 
                      play (empty size) O (gametree (empty size) O size) size
                  else 
                      play (empty size) X (gametree (empty size) X size) size
              else 
                  do putStrLn "ERROR: You need to put 1 or 2"
                     main
          else 
              do putStrLn "ERROR: You must to put a number"
                 main

play :: Grid -> Player -> Tree Grid -> Int -> IO ()
play g p t size = do cls
                     goto (1,1)
                     putGrid size g 
                     play' g p t size

play' :: Grid -> Player -> Tree Grid -> Int -> IO ()
play' g p t size | wins O g size = putStrLn "Player O wins ! \n"
                 | wins X g size = putStrLn "Player X wins ! \n"
                 | full g    = putStrLn "It's a draw! \n"
                 | p == O    = do i <- getNat (prompt p)
                                  case move g i p size of 
                                      [] -> do putStrLn "ERROR: Invalid move: "
                                               play' g p t size
                                      [g'] -> play g' (next p) (serchandremp t g') size
                  | p == X    = do putStr "Player X is thinking ... \n"
                                   let g' = bestmove' t p size
                                   let t' = serchandremp t g'
                                   play g' (next p) t' size
