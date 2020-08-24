module Ejercicios_cap8 where

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']
--find 'a' (zip "abcdbca" [1..10])

type Pos = (Int,Int)
type Trans = Pos -> Pos

data Move = North | South | East | West

move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

--data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

--newtype Nat = N Int
--type Nat = Int
--data Nat = N Int

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add m n = int2nat (nat2int m + nat2int n)

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs:: Eq a => a -> Tree a ->Bool
occurs x (Leaf y) = x==y
occurs x (Node l y r) = x==y || occurs x l || occurs x r 

flatten:: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) =  flatten l ++ [x] ++ flatten r 

occurs2:: Ord a => a -> Tree a ->Bool
occurs2 x (Leaf y)                  = x==y
occurs2 x (Node l y r) | x==y       = True
                       | x<y        = occurs x l 
                       | otherwise  = occurs x r 




--8_9_1:
mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ m) n = add n (mult m n) 


--8_9_2:
{-occurs2 :: Ord a => a -> Tree a ->Bool
occurs2 x (Leaf y)                  = x==y
occurs2 x (Node l y r) | x==y       = True
                       | x<y        = occurs x l 
                       | otherwise  = occurs x r -}

occurs3 :: Ord a => a -> Tree a ->Bool
occurs3 x (Leaf y)               = x==y
occurs3 x (Node l y r) = case compare x y of 
                          EQ      -> True
                          LT      -> occurs x l 
                          GT      -> occurs x r  





--8_9_3:
data Arbol a = Rama a | Nodo (Arbol a) (Arbol a)
arb :: Arbol Int
arb = Nodo (Nodo (Rama 5 )(Rama 7 ))(Nodo (Rama 8 )(Nodo (Nodo (Rama 14 )(Rama 26 ))(Rama 13 )))

arb2 :: Arbol Int
arb2 = Nodo (Nodo (Rama 5 )(Rama 7 ))(Nodo (Rama 8 )(Rama 13 ))


numleav:: Arbol a -> Int
numleav (Rama a) = 1
numleav (Nodo a b) = numleav a + numleav b

balanced :: Arbol a -> Bool
balanced (Rama r) = True
balanced (Nodo i d) =abs (numleav i - numleav d) <= 1 && balanced i && balanced d




--8_9_4:
halve :: [a]->([a],[a])
halve [] = ([],[])
halve x = (first, second)
    where
        h=(length x) `div` 2
        first = take h x
        second = drop h x

balance :: [a] -> Arbol a
balance xs | length xs == 1  = Rama (head xs)
           | otherwise       = Nodo (balance (fst (halve xs))) (balance (snd (halve xs))) 




--8_9_5:
data Expr = Val Int | Add Expr Expr 

value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)

-- folde (+5) (\x y -> x*2+y*3) (Add (Add (Val 2) (Val 3)) (Val 4))
-- 2     3 4 
-- 7     8 9 
-- 7*2+8*3 9
-- 38*2 + 9*3
-- 103 

--folde show (++) (Add (Add (Val 2) (Val 3)) (Val 4))



--8_9_6:

eval2 :: Expr -> Int
eval2 = folde id (+)     --103:104

size :: Expr -> Int
size = folde (const 1) (+)



--8_9_7:
{-instance Eq a => Eq (Maybe a) where
    (==), (/=) :: a -> a -> Bool
    Just x == Just y = x == y
    (Just x) /= (Just y) = not (Just x == Just y)


instance Eq a => Eq [a] where
    (==), (/=) :: a -> a -> Bool
    x == y = all map (==)   
    (Just x) /= (Just y) = not (Just x == Just y)
-}




-- 8_9_8:

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Dis Prop Prop           --Agregado
          | Equiv Prop Prop         --Agregado

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')
p5 :: Prop
p5 = Not(Equiv (Dis (Var 'P')(Var 'Q'))(Not (Dis (Var 'P')(Var 'Q'))) ) 


type Subst = Assoc Char Bool 

eval :: Subst -> Prop -> Bool
eval _ (Const b)    = b 
eval s (Var x)      = find x s 
eval s (Not p)      = not (eval s p)
eval s (And p q)    = eval s p && eval s q 
eval s (Imply p q)  = eval s p <= eval s q  
eval s (Dis p q)    = eval s p || eval s q                         --Agregado
eval s (Equiv p q)  = eval s p == eval s q                         --Agregado

vars :: Prop -> [Char]
vars (Const _)      = []
vars (Var x)        = [x]
vars (Not p)        = vars p
vars (And p q)      = vars p ++ vars q
vars (Imply p q)    = vars p ++ vars q
vars (Dis p q)      = vars p ++ vars q  --Agregado
vars (Equiv p q)    = vars p ++ vars q  --Agregado

int2bin :: Int -> [Int]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

bools :: Int -> [[Bool]]
bools n = map (reverse . map conv . make n . int2bin ) range 
    where
        range = [0..(2^n)-1]
        make n bs = take n (bs ++ repeat 0)
        conv 0 = False  
        conv 1 = True

bools2 :: Int -> [[Bool]]
bools2 0 = [[]]
bools2 n = map (False:) bss ++ map (True:) bss
    where bss = bools2 (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
    where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]


-- 8_9_9:
data Expr2 = Val2 Int | Add2 Expr2 Expr2 | Mult2 Expr2 Expr2

value2 :: Expr2 -> Int
value2 (Val2 n) = n
value2 (Add2 x y) = value2 x + value2 y
value2 (Mult2 x y) = value2 x * value2 y


-- value2 (Add2 (Mult2 (Val2 2) (Val2 3)) (Val2 4))

