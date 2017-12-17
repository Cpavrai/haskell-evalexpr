module MainFunc
    ( mainFunc
    ) where

import System.Environment
import Data.List

mainFunc :: IO ()

mainFunc = do
    args <- getArgs
    print args
{-   args <- getArgs
   mapM putStrLn args
-}