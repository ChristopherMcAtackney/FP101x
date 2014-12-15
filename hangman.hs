import Data.Char
import System.IO

getCh :: IO Char
getCh = do c <- getChar
           return c

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
               do putChar x
                  return []
               else
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)

guess :: String -> IO ()
guess word =
  do putStr "> "
    xs <- getLine
	if xs == word then
	  putStrLn "You got it!"
	else
	  do putStrLn (diff word xs)
	     guess word