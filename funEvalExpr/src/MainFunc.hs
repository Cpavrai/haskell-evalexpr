module MainFunc
    ( mainFunc
    ) where

import System.Environment
import Data.List
import Calculate

mainFunc :: IO ()

mainFunc = do
    args <- getArgs
    case length args of
        0 -> error "Need an argument"
        1 -> calculate (args !! 0)
        _ -> error "Only argument needed"
