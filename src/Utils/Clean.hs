module Utils.Clean
    ( clean
    ) where

clean :: String -> String
clean xs = if length xs <= 1
              then xs
              else take 1 xs ++ " " ++ clean (drop 1 xs)