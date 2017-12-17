module MainFunc
    ( mainFunc
    ) where

import System.Environment
import Data.List

mainFunc :: IO ()

mainFunc = do
    args <- getArgs
    if null args
        then print "Need one argument atleast"
    else do
        print (args !! 1)
        print (args !! 2)
{-    if args[1] == '+'
        then print add args
        else
            print "New fonction to code"
-}