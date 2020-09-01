module Ejercicios_cap12 where


--class Applicative m => Monad m where
    --return :: a -> m a
    --(>>=) :: m a -> (a -> m b) -> m b

--12_1:
inc2 :: [Int] -> [Int]
inc2 []     = []
inc2 (n:ns) = n+1 : inc ns

sqr :: [Int] -> [Int]
sqr []     = []
sqr (n:ns) = n^2 : sqr ns

map2 :: (a -> b) -> [a] -> [b]
map2 f []     = []
map2 f (x:xs) = f x : map2 f xs

inc3 = map (+1)
sqr2 = map (^2)

--class Functor2 f where
--    fmap :: (a -> b) -> f a -> f b

--instance Functor [] where
    -- fmap :: (a -> b) -> [a] -> [b]
    -- fmap = map

--data Maybe a = Nothing | Just a
--instance Functor Maybe where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    -- fmap _ Nothing = Nothing
    -- fmap g (Just x) = Just (g x)

data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving Show

instance Functor Tree where
-- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf x)   = Leaf (g x)
    fmap g (Node l r) = Node (fmap g l) (fmap g r)

inc :: Functor f => f Int -> f Int
inc = fmap (+1)

--instance Functor [] where
-- fmap :: (a -> b) -> f a -> f b
--    fmap g [] = []
--    fmap g (x:xs) = fmap g xs ++ [g x]

prods :: [Int] -> [Int] -> [Int]
prods xs ys = pure (*) <*> xs <*> ys

getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n-1)


sequenceA2 :: Applicative f => [f a] -> f [a]
sequenceA2 []     = pure []
sequenceA2 (x:xs) = pure (:) <*> x <*> sequenceA2 xs

data Expr = Val Int | Div Expr Expr
{-
eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y
-}
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = case eval x of
    Nothing -> Nothing
    Just n -> case eval y of
        Nothing -> Nothing
        Just m  -> safediv n m


--eval2 :: Expr -> Maybe Int
--eval2 (Val n) = pure n
--eval2 (Div x y) = pure safediv <*> eval2 x <*> eval2 y

eval3 :: Expr -> Maybe Int
eval3 (Val n) = Just n
eval3 (Div x y) = eval3 x >>= \n ->
                  eval3 y >>= \m ->
                  safediv n m


eval4 :: Expr -> Maybe Int
eval4 (Val n) = Just n
eval4 (Div x y) = do n <- eval4 x
                     m <- eval4 y
                     safediv n m

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x,y)

pairs2 :: [a] -> [b] -> [(a,b)]
pairs2 xs ys = [(x,y) | x <- xs, y <- ys]

--pairs3 :: [a] -> [b] -> [(a,b)]
--pairs3 xs ys = xs >>=  = [(x,y) | x <- xs, (x,y) <- f x]


--type ST = State -> State

--type ST a = State -> (a,State)

type State = Int

newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (\s -> (x,s))

    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = S (\s ->
        let (f,s') = app stf s
            (x,s'') = app stx s' in (f x, s''))

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

--data Tree a = Leaf a | Node (Tree a) (Tree a)
--            deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
    where
        (l',n') = rlabel l n
        (r',n'') = rlabel r n'


fresh :: ST Int
fresh = S (\n -> (n, n+1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _)   = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r


mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do n <- fresh
                     return (Leaf n)
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')








--12_5_1:
class Functor' f where
  fmap' :: (a -> b) -> f a -> f b


data Tree2 a = Leaf2 | Node2 (Tree2 a) a (Tree2 a)
    deriving Show
instance Functor' Tree2 where
-- fmap' :: (a -> b) -> Tree2 a -> Tree2 b
    fmap' g Leaf2         = Leaf2
    fmap' g (Node2 l x r) = Node2 (fmap' g l) (g x) (fmap' g r)







--12_5_2:
instance Functor' ((->) a) where
    --fmap' :: ( c -> b ) -> (a -> c) -> ( a -> b)
    fmap' g h = g . h






--12_5_3:

class Functor' f => Applicative' f where
  pure' :: a -> f a
  (<**>) :: f (a -> b) -> f a -> f b

instance Applicative' ((->) a) where
    --pure' :: b -> (a -> b)
    pure' x = undefined--const x --(\a -> x)
    --(<**>) :: (a ->  (c -> b) ) -> (a -> c) -> (a -> b)
    f <**> g = undefined-- \x -> f x (g x)




--12_5_4:

newtype ZipList a = Z [a] deriving Show

instance Functor' ZipList where
    --fmap' :: (a -> b) -> ZipList a -> ZipList b
    fmap' g (Z xs) = Z [g x | x <- xs]

instance Applicative' ZipList where
    --pure' :: a -> ZipList a
    pure' x =  Z [x]

    --(<**>) :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z gs) <**> (Z xs) = Z [ g x | (x,g) <- zip xs gs]









--12_5_5:

-- Como (<*>) :: Aplicative f => f (a -> b) -> f a -> f b
-- Entonces pure id <*> :: Aplicative f => f a -> f a  (Que es id en a)
-- (pure id <*>) x = x

-- Como (<*>) :: Aplicative f => f (a -> b) -> f a -> f b
-- Entonces en pure g <*> pure x, pure g :: Aplicative f => f (a -> b)
--                                pure x :: Aplicative f => f a
-- Y esta f es pure







--12_5_6:
class Applicative' f => Monad' f where
  (>>==) :: f a -> (a -> f b) -> f b

instance Monad' ((->) a) where
    --(>>==) :: (a -> b) -> (b -> ( a -> c)) -> ( a -> c)
    f >>== g = \x -> g (f x) x






--12_5_7:
data Expr2 a = Var2 a | Val2 Int | Add2 (Expr2 a) (Expr2 a)
    deriving Show

instance Functor' Expr2 where
    -- fmap' :: (a -> b) -> Expr2 a -> Expr2 b
    fmap' g (Var2 x)   = Var2 (g x)
    fmap' _ (Val2 x)   = Val2 x
    fmap' g (Add2 l r) = Add2 (fmap' g l) (fmap' g r)

instance Applicative' Expr2 where
    --pure :: a -> Expr2 a
    pure' a = Var2 a

    -- (<**>) :: Expr2 (a -> b) -> Expr a -> Expr2 b
    (<**>) (Var2 g) (Val2 x)   = Val2 x
    (<**>) (Var2 g) (Var2 x)   = Var2 (g x)
    (<**>) (Val2 y) (Val2 x)   = Add2 (Val2 y) (Val2 x)
    (<**>) (Val2 y) _          = Val2 y
    (<**>) (Add2 g h) x        = Add2 (g <**> x) (h <**> x)
    (<**>) (Var2 g) (Add2 x y) = Add2 ((Var2 g) <**> x) ((Var2 g) <**> x)


instance Monad' Expr2 where
    --(>>==) :: f a -> (a -> f b) -> f b
    (Val2 x) >>== g     = Val2 x
    (Var2 x) >>== g     = g x
    (Add2 x y) >>== g   = Add2 (x >>== g) (y >>== g)







--12_5_8:
{-
type State = Int

newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (\s -> (x,s))

    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = S (\s ->
        let (f,s') = app stf s
            (x,s'') = app stx s' in (f x, s''))

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')
-}

-- type State = Int

-- newtype ST a = S (State -> (a,State))

-- app :: ST a -> State -> (a,State)
-- app (S st) x = st x

-- (>>==) :: ST a -> (a -> ST b) -> ST b
-- do expr1
--    x <- expr2
--    expresiones x

instance Functor' ST where
  -- fmap' :: (a -> b) -> ST a -> ST b
  fmap' g st = do x <- st
                  S (\s -> (g x,s))
  -- fmap' g st = st >>== (\a -> S (\s->(g a,s)) )



--ST a :: S (State -> (a,State))

instance Applicative' ST where
  -- pure :: a -> ST a
  pure' x = S (\s -> (x,s))
  -- (<**>) :: ST (a -> b) -> ST a -> ST b

  stf <**> stx = do
    let f = (\a -> let (g,s') = app stf undefined in S(\s->(g a,s) ) )
    let solution = stx >>== f
    solution


instance Monad' ST where
  -- (>>==) :: ST a -> (a -> ST b) -> ST b
  st >>== f = S (\s -> let (x,s') = app st s in app (f x) s')



















































------------------ Visto como ejemplo---------
{-
data List' a = Null' | Cons' a (List' a)

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' List' where
  fmap' f (Null')      = Null'
  fmap' f (Cons' x xs) = Cons' (f x) (fmap' f xs)

class Functor' f => Applicative' f where
  pure' :: a -> f a
  (<**>) :: f (a -> b) -> f a -> f b

instance Applicative' List' where
  pure' x = Cons' x Null'
  (<**>) Null' _                   = Null'
  (<**>) _ Null'                   = Null'
  (<**>) (Cons' g gs) (Cons' x xs) = Cons' (g x) (gs <**> xs)

class Applicative' f => Monad' f where
  (>>==) :: f a -> (a -> f b) -> f b



instance Monad' List' where
  (>>==) Null' _        = Null'
  (>>==) (Cons' x xs) f = case f x of
                            Null' -> Null'
                            Cons' y ys -> Cons' y ys
                            Cons' y Null' -> Cons' y (xs >>== f)








  -- ls | cat :
  --    (>>==) :: IO [String] -> ([String] -> IO ()) -> IO ()


-- -----------
-- C
-- A, B, C
-- f : A -> B
-- ----------

-- -----------
-- C
-- A, B, C
-- f : A -> B
-- ----------

-- D
-- F A, F B, F C
-- F f : F A -> F B
-- ----------


-- F : C -> D

-- foo :: List' Int

-- data State s a = S (s -> (s, a))
-}
