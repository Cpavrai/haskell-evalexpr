module Calculate
    ( calculate
    ) where

import Add
import Sub

do_calculate elem = do
    let first = read (head elem)::Int
    let second = read (elem !! 2)::Int
    case elem !! 1 of
        "+" -> print (add first second)
        "-" -> print (sub first second)
        _ -> print "Another operation"

calculate elem = do
    let word_tab = words elem
    case length word_tab of
        0 -> error "Invalid argument"
        1 -> print (word_tab !! 0)
        _ -> do_calculate word_tab
