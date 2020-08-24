module Ejercicio_cap9_6 where

data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Exp = "^"
    
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

--Sacado de ejercicio 9_9
valid :: Op -> Int -> Int -> Bool
valid Add x y = x<=y
valid Sub x y = x > y
valid Mul x y = x > 1 && y > 1 && x<=y 
valid Div x y = y > 1 && x `mod` y == 0
valid Exp x y = y > 1 && x > 1

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


--9_6:
split :: [a] -> [([a],[a])]
split []        = []
split [_]       = []
split (x:xs)    = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

-- Ejemplo: split [1,2,3,4]

ops :: [Op]
ops = [Add,Sub,Mul,Div,Exp]

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


sol=solutions' [1,3,7,10,25,50] 765


-- solutions [1,2,5] 25


easy :: [Expr]->[Expr]
easy [e] =  [e]
easy (e:es) = smaller ++ [e] ++ taller
    where 
        smaller = [e' | e'<-es , length (show e')<=length (show e)]
        taller  = [e' | e'<-es , length (show e')>length (show e)]

minimo2 :: [(a,Int)] -> (a,Int)
minimo2 [(a,n)] = (a,n)
minimo2 ( (a,n) :xs ) = ss
    where   
        solu = minimo2 xs
        ss = if n <= (snd solu) then (a,n) else solu

--solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = if null s2s then [fst(minimo2 s1s)] else easy s2s
    where   
        s1s = [(e,abs(n-m)) | ns' <- choices ns, (e,m) <- results ns']
        s2s = [e |  (e,m)<-s1s , m == 0] 



