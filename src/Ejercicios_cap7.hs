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

-- 7_9_2:
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
-- 7_9_3:
map2 :: (a -> b) -> [a] -> [b]
--map2 f (x:xs) = f x : map f xs
map2 f xs = foldr (\x y -> f x : y ) [] xs 

filter2 :: (a -> Bool) -> [a] -> [a]
--filter2 p xs = [x | x <- xs, p x]
filter2 p = foldr (\x y -> if (p x) then x:y else y ) []



-- 7_9_4:
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> x*10 + y) 0   



-- 7_9_5:
suma ::Num a => (a,a)->a
suma (x, y) = x+y

suma2 ::Num a => a->a->a
suma2 x y = x+y

curry2 :: ((a,b)->c)-> a->b->c 
curry2 f a b = f (a,b)

uncurry2 :: (a->b->c)->((a,b)->c) 
uncurry2 f xs = f (fst xs) (snd xs) 



-- 7_9_6:
--Anamorfismo      fold: catamorfismo
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

chop8' :: [Bit] -> [[Bit]]
--chop8 [] = []
--chop8 bits = take 8 bits : chop8 (drop 8 bits)
chop8' = unfold null (take 8) (drop 8) 

map3 :: (a -> b) -> [a] -> [b]
--map f [] = []
--map f (x:xs) = f x : map f xs
--                  p       h     t   x
map3 f xs = unfold null (f.head) tail xs

iterate2 f x = unfold (const True) f f x
-- Ejemplo: zip [1..10](iterate2 (*2) 1)



-- 7_9_7:8:
type Bit = Int
bin2int :: [Bit] -> Int
--bin2int bits = sum [w*b | (w,b) <- zip weights bits]
--    where weights = iterate (*2) 1
bin2int = foldr (\x y -> x + 2*y) 0
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)
make8 :: [Bit] -> [Bit]
--make8 bits = take 8 (bits ++ repeat 0)
make8 bits = (sum bits `mod` 2 ) : (take 8 (bits ++ repeat 0))

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)
--Ejemplo: encode "Rain caTs and d0g5"

chop9 :: [Int] -> [[Int]]
chop9 [] = []
--chop8 bits = take 8 bits : chop8 (drop 8 bits)
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode :: [Bit] -> String
--decode = map (chr . bin2int) . chop8
decode xs | p = map (chr . bin2int) x8s
          | otherwise = error "Hubo un error en la transimision"
    where 
        x8s = [ t |(h:t)<-chop9 xs]
        p = all (==True) [sum t `mod` 2 == h|(h:t)<-chop9 xs]

{-
decode xs | all (==True) [sum t `mod` 2 == h|(h:t)<-chop9 xs] = map (chr . bin2int) (map tail (chop9 xs))
          | otherwise = error "Hubo un error en la transimision"
-}
transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id
--channel = tail

--Ejemplo:    transmit "Rain caTs and d0g5"



-- 7_9_9:
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x :  altMap g f xs

-- 7_9_10:
luhnDouble :: Int -> Int
luhnDouble x = if 2*x >= 10 then 2*x - 9 else 2*x 

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z w = if (luhnDouble x + y +luhnDouble z + w) `mod` 10 == 0 then True else False

luhn2 :: [Int] -> Bool
luhn2 xs = sum (altMap luhnDouble id xs) `mod` 10 ==0

--Ejemplos del libro: luhn2 [1,7,8,4]           =True
--Ejemplos del libro: luhn2 [4,7,8,3]           =False
--Ejemplo tarjeta real: luhn2 [5,3,3,3,6,1,9,5,0,3,7,1,5,7,0,2]















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