module Calculate
    ( calculate
    ) where

do_calculate elem = do
    print "Let's code it !"

calculate elem = do
    let word_tab = words elem
    case length word_tab of
        0 -> error "Invalid argument"
        1 -> print (word_tab !! 0)
        _ -> do_calculate word_tab
