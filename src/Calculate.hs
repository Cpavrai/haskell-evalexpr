module Calculate
    ( calculate
    ) where

import Data.List
import Text.Printf
import Parser
import Translater

compute :: Interpretation -> Double

compute (Addition operator_one leftOne rightOne) | operator_one == Add = (compute leftOne) + (compute rightOne)
                                                 | operator_one == Subs = (compute leftOne) - (compute rightOne)

compute (Multiplication operator_one leftOne rightOne) | operator_one == Mul = (compute leftOne) * (compute rightOne)
                                                       | operator_one == Div = let divider = compute rightOne in
                                                                        case divider of 
                                                                        divider | divider /= 0.0 -> (compute leftOne) / divider
                                                                        otherwise -> error ("Division by zero")

compute (Number nb) = nb

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