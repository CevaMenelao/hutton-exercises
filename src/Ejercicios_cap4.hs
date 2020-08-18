module Ejercicios_cap4 where

abs2 n | n>=0 = n 
       | otherwise = -n

test :: [Char]->Bool
test ['a',_,_] = True
test _ = False
odds :: Int -> [Int]
odds n = map f [0..n-1]
    where f x = x*2 + 1

-- 4_8_1:
halve :: [a]->([a],[a])
halve x = (first, second)
    where
        h=(length x) `div` 2
        first = take h x
        second = drop h x
-- 4_8_2:
third :: [a] -> a
--third x = x!!2
--third x = head (drop 2 x)
--third x = head (tail (tail x))
third  (_:_:x:_)=x
-- 4_8_3:
safetail:: [a] -> [a]
--safetail xs = if null xs then [] else tail xs
--safetail xs | null xs = [] 
--            | otherwise = tail xs
safetail [] = []
safetail (_:xs) = xs
-- 4_8_4:
--(||||) :: Bool -> Bool -> Bool
--True |||| True = True
--True |||| False = True
--False |||| True = True
--False |||| False = False

--False ||||False = False
--_||||_ = True

--False |||| b = b
--True |||| _ = True

--b |||| b =b 
--_ |||| _ = True
-- 4_8_5:
(&&&):: Bool -> Bool ->Bool
--a &&& b = if a==True then (if b==True then True else False) else False 

-- 4_8_6: 
a &&& b = if a==True then b else False 

-- 4_8_7: 
mult :: Int -> Int -> Int -> Int
mult = \x -> ( \y -> (\z -> x*y*z))
-- 4_8_8:
luhnDouble :: Int -> Int
luhnDouble x = if 2*x >= 10 then 2*x - 9 else 2*x 
luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z w = if (luhnDouble x + y +luhnDouble z + w) `mod` 10 == 0 then True else False

