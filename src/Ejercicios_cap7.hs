module Ejercicios_cap7 where

import Data.Char
import Data.List

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr f v xs)

sum2 :: Num a => [a] -> a
sum2  = foldr (+) 0 

sum3 :: Num a => [a] -> a
sum3 = foldl (+) 0
--foldr (#) v [x0,x1,...,xn] = x0 # (x1 # (... (xn # v) ...))
--foldl (#) v [x0,x1,...,xn] = (... ((v # x0) # x1) ...) # xn

type Bit = Int

bin2int :: [Bit] -> Int
--bin2int bits = sum [w*b | (w,b) <- zip weights bits]
--    where weights = iterate (*2) 1

bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id



votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

ballots :: [[String]]
ballots = [["Red", "Green"], ["Blue"], ["Green", "Red", "Blue"], ["Blue", "Green", "Red"], ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])
elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))
rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head
winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of 
    [c] -> c
    (c:cs) -> winner' (elim c bs)


-- 7_9_1:
-- Expresión: [f x | x <- xs, p x]
--            map f (filter p xs ) 

-- 7_9_2_a:
all2 ::(a -> Bool) -> [a] -> Bool
all2 p xs = foldr (&&) True (map p xs)  

any2 ::(a -> Bool) -> [a] -> Bool
any2 p xs = foldr (||) False (map not (map p xs))  



takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 _ [] = []
takeWhile2 p (x:xs) | p x  = x : takeWhile p xs
                    | otherwise = []

dropWhile2 :: (a -> Bool) -> [a] -> [a]
dropWhile2 _ [] = []
dropWhile2 p (x:xs) | p x  = dropWhile2 p xs
                    | otherwise = xs

map2 :: (a -> b) -> [a] -> [b]
map2 f [] = []
--map2 f (x:xs) = f x : map f xs
map2 foldr (\x y -> x : f y ) [] xs





















--takeWhile2 p xs = take head( positions False (map p xs)) xs 
--takeWhile2 p xs = foldr ( \x y -> if p y then x:y else x:[]) [] xs
--p x 
{-map p xs 
filter p xs 
map tail.inits xs
p.head xs 
map p.head xs 
foldr (\x y -> x && y) True  (map p xs) 
filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p xs = [x | x <- xs, p x]-}