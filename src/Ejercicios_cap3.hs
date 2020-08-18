module Ejercicios_cap3 where

x=if True then 1 else 3

add :: (Int,Int) -> Int
add (x,y) = x+y
zeroto :: Int -> [Int]
zeroto n = [0..n]

add' :: Int -> (Int -> Int)
add' x y = x+y

adicion :: (Int,Int) -> Int
adicion (x,y) = x+y

--mult :: Int -> (Int -> (Int -> Int))
--mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z

--add' 1 :: Int -> Int
-- 3_11_2:
bools=[True,False,False,True]
nums=[[1,2],[3,4,5],[6,7,8,9],[0]]
add_3 x y z = x+y+z
copy x = (x,x)
applay f x = f x

-- 3_11_3:
second xs = head (tail xs)
pair x y = (x,y)
double' x = 2 * x
palindome xs = reverse xs == xs
twice f x = f (f x)


