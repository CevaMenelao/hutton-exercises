module Ejercicios_cap6 where

-- 6_8_1:
fac :: Int ->  Int
fac 0 = 1
fac n | n >=0 = n * fac (n-1)
      | otherwise = error "No esta definida la funcion fac para negativos"
    
-- 6_8_2:
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n|n>=0 = n + sumdown (n-1)

-- 6_8_3: 
(^^^) :: Int -> Int -> Int
m ^^^ 0 = 1
m ^^^ n = m * m^(n-1) 

-- 6_8_4: 
euclid :: Int -> Int -> Int
euclid 0 m = m
euclid n 0 = n
euclid n m = euclid m (n `mod` m)

(!!!) :: [a] -> Int -> Maybe a
[]!!!_ = Nothing
xs!!!0= Just $ head xs
(x:xs)!!!n = xs!!!(n-1)


-- 6_8_7:
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs []  = xs
merge (x:xs) (y:ys) | x<=y  = [x] ++ merge xs (y:ys)
merge (x:xs) (y:ys) | x>=y  = [y] ++ merge (x:xs) ys

-- 6_8_8:
halve :: [a]->([a],[a])
halve [] = ([],[])
halve x = (first, second)
    where
        h=(length x) `div` 2
        first = take h x
        second = drop h x

msort :: Ord a => [a] -> [a]
msort [a]  = [a] 
msort xs = merge (msort( fst (halve xs)))  (msort(snd (halve xs)))

-- 6_8_9:
sum2 ::Num a => [a] -> a
sum2 [] = 0
sum2 (x:xs) = x + sum xs

take2 :: Int-> [a]->[a]
take2 0 _ = []
take2 _ [] = []
take2 n (x:xs) = x : take2 (n-1) xs

last2 :: [a]-> a
last2 (x:xs) = if null xs then x else last2 xs