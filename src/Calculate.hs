module Calculate
    ( calculate
    ) where

import Data.List
import Text.Printf
import Compute
import Translater
import Parser

do_calculate :: String -> Double
do_calculate = compute . translater . parser

calculate elem = do
    let word_tab = words elem
    let digi = []
    let oper = []
    case length word_tab of
        0 -> error "Invalid argument"
        1 -> print (word_tab !! 0)
        _ -> printf "%.2f" (do_calculate elem)