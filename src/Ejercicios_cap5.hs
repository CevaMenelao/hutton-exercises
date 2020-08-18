module Ejercicios_cap5 where

import Data.Char



let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m)*100

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']
count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
    where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
    where
        factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs


--5_7_1:
-- Expretion: [x^2 | x <- [1..100]]

--5_7_2:
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

--5_7_3:
square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y)<-grid n n, x /= y]

--5_7_4:
replicate2 :: Int -> a -> [a]
replicate2 n b = [b | _ <- [1..n]]

--5_7_5:
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x<- [1..n], y<- [1..n],z<- [1..n], x^2+y^2==z^2]

--5_7_6:
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
perfects :: Int -> [Int]
perfects n = [x | x <- [2..n], sum (factors x) == 2*x ]

--5_7_7:
--[(x,y) | x <- [1,2], y <- [3,4]]
--Forma 1 concat [ [(1,y) | y <- [3,4]] , [(2,y) | y <- [3,4]] ]
--[(1,y) | y <- [3,4]] ++ [(2,y) | y <- [3,4]] 
--Forma 2 concat [ [(x,y) | y <- [3,4]] | x<-[1,2] ]

--5_7_8:
--positions x xs = [i | (x',i) <- zip xs [0..], x == x']
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']
--Ejemplo find: find ’b’ [(’a’,1),(’b’,2),(’c’,3),(’b’,4)]
--[2,4]
--Ejemplo psoitios: positions False [True, False, True, False]
--[1,3]

positions2 :: Eq a => a -> [a] -> [Int]
positions2 x xs = find x (zip xs [0..]) 

--5_7_9:
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]







--5_7_10:


shift2 :: Int -> Char -> Char
shift2 n c | isLower c = int2let ((let2int c + n) `mod` 26)
           | isUpper c = int2let (((let2int c + n+32) `mod` 26)-32)
           | otherwise = c

encode2 :: Int -> String -> String
encode2 n xs = [shift2 n x | x <- xs]

u2l:: Char ->Char
u2l c |  isUpper c = int2let (let2int c +32)
      |  otherwise = c

l2u:: Char ->Char
l2u c |  isLower c = int2let (let2int c -32)
      |  otherwise = c

crack2 :: String -> String
crack2 xs =  [ if p then l2u x else x | (p,x) <- zip grandes (encode (-factor) ys)]
    where
        grandes = map isUpper xs
        factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs ys
        ys = [u2l c | c <- xs]

--"This is an example to present in my computer"
--"ThIs Is an exAmple to pResent in my coMputEr"