module Ejercicios_cap10 where

act2 :: IO (Char,Char)
act2 = do   x <- getChar 
            y <- getChar
            return (x,y)



getLine2 :: IO String
getLine2 = do x <- getChar
              if x == '\n' then 
                  return [] 
              else do xs <- getLine2
                      return (x:xs)


putStr2 :: String -> IO ()
putStr2 [] = return ()
putStr2 (x:xs) = do putChar x
                    putStr2 xs

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " chacters"



sgetLine :: IO String 
sgetLine = do x <- getChar
              if x == '\n' then 
                 do putChar x 
                    return [] 
              else 
                  do  putChar '-'
                      xs <- sgetLine
                      return (x:xs)

{-
hangman :: IO ()
hangman = do putStrLn "Think of a word:"
             word <- sgetLine
             putStrLn "Try to guess it:"
             play word-}