module Ejercicios_cap13 where
    
import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x,xs)])

instance Functor Parser where
-- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
                    [] -> []
                    [(v,out)] -> [(g v, out)])


instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v,inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
        [] -> []
        [(g,out)] -> parse (fmap g px) out)

--three :: Parser (Char,Char)
--three = pure g <*> item <*> item <*> item
--    where g x y z = (x,z)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
        [] -> []
        [(v,out)] -> parse (f v) out)

three :: Parser (Char,Char)
three = do x <- item
           item
           z <- item
           return (x,z)

class Applicative f => Alternative' f where
    empty' :: f a
    (<||>) :: f a -> f a -> f a

instance Alternative' Maybe where
    -- empty :: Maybe a
    empty' = Nothing
    -- (<|>) :: Maybe a -> Maybe a -> Maybe a
    Nothing <||> my = my
    (Just x) <||> _ = Just x