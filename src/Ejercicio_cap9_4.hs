module Ejercicio_cap9_4 where

data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

--9_3:
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n)     = show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where 
                           brak (Val n) = show n
                           brak e       = "(" ++ show e ++")" 

values :: Expr -> [Int]
values (Val n)      = [n]
values (App o l r)  = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)        = [n | n>0 ]
eval (App o l r)    = [apply o x y | x <- eval l,  y <- eval r, valid o x y] 

--9_4:
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
    where
        yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a]->[[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

--Todas las sublistas posibles en cualquier orden
choices :: [a] -> [[a]]
choices = concat . map perms . subs

--Ejemplos:     subs [1,2,3]
--              interleave 1 [2,3,4]
--              perms [1,2,3]

--9_5:
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

--9_6:
split :: [a] -> [([a],[a])]
split []        = []
split [_]       = []
split (x:xs)    = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

-- Ejemplo: split [1,2,3,4]

ops :: [Op]
ops = [Add,Sub,Mul,Div]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

--Todas las expresiones posibles de una lista de enteros
exprs :: [Int] -> [Expr]
exprs []    = []
exprs [n]   = [Val n]
exprs ns    = [e | (ls,rs)  <- split ns,
                   l        <- exprs ls,
                   r        <- exprs rs,
                   e        <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

--9_7:
main :: IO ()
main = print (solutions [1,3,7,10] 73)

--9_8:
type Result = (Expr,Int)

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n , n) | n>0]
results ns  = [res | (ls,rs) <- split ns,
                      lx     <- results ls,
                      ry     <- results rs,
                      res    <- combine' lx ry]  


combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

--9_9:

sol=solutions' [1,3,7,10,25,50] 765

--9_9_4:
pos ns = (y1s,y2s)
   where

        y1s= sum[1 | x<-choices ns, _<-exprs x]
        y2s= sum[if null (eval y) then 0 else 1 | x<-choices ns, y<-exprs x]

--  5+4     4+5     5-4     4-5     4*5     
--  5*4     5/4     4/5     4       5
-- 
--  5-4     5*4     4*5     5+4     4+5     
--  5       4 


--  2+1     1+2     2-1     1-2     1*2     
--  2*1     2/1     1/2     1       2
-- 
--  2-1     2*1     1*2     2+1     1+2     
--  2       1       2/1
