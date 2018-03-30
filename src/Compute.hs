module Compute 
    ( compute
    )
    where

import Translater
import Parser

compute :: Interpretation -> Double

compute (Addition operator_one leftOne rightOne) | operator_one == Add = (compute leftOne) + (compute rightOne)
                                                 | operator_one == Subs = (compute leftOne) - (compute rightOne)

compute (Multiplication operator_one leftOne rightOne) | operator_one == Mul = (compute leftOne) * (compute rightOne)
                                                       | operator_one == Div = let divider = compute rightOne in
                                                            case divider of
                                                                divider | divider /= 0.0 -> (compute leftOne) / divider
                                                                otherwise -> error ("Division by zero")

compute (Powering operator_one leftOne rightOne) | operator_one == Pow = (compute leftOne) ** (compute rightOne)
                                                 | operator_one == Root = let to_root = compute leftOne in
                                                    case to_root of
                                                        to_root | to_root >= 0.0 -> (compute rightOne) ** to_root
                                                        otherwise -> error ("Only root of positive numbers is possible")
compute (Ending op expr) | op == Add = compute expr
                         | op == Subs = 0 - (compute expr)

compute (Number nb) = nb