module Calculate
    ( calculate
    ) where

import Data.List
import Text.Printf
import Add
import Sub
import Mul
import Div
import Pow

add_oper :: [Double] -> [String] -> [String] -> ([Double], [String])
add_oper digi oper elem = case elem of
    [] -> ((reverse digi), (reverse oper))
    _ -> add_digi digi ((head elem):oper) (tail elem)

add_digi :: [Double] -> [String] -> [String] -> ([Double], [String])
add_digi digi oper elem = add_oper ((read (head elem)::Double):digi) oper (tail elem)

do_calculate :: ([Double], [String]) -> IO()
do_calculate elem = case elem of
    (digit, operators) -> case (findIndex (== "^") operators) of
        Nothing -> case (findIndex (== "/") operators) of
            Nothing -> case findIndex (== "*") operators of
                Nothing -> case findIndex (== "-") operators of
                    Nothing -> case findIndex (== "+") operators of
                        Nothing -> do
                            let result = head digit
                            putStrLn . printf "%.2f" $ result
                        Just n -> do
                            let adder = add (digit!!n) (digit!!(n+1))
                            let (ys,zs) = splitAt n digit
                            let (bs,cs) = splitAt n operators
                            do_calculate ((ys ++ adder:(tail (tail zs))), bs++(tail cs))
                    Just n -> do
                        let suber = sub (digit!!n) (digit!!(n+1))
                        let (ys,zs) = splitAt n digit
                        let (bs,cs) = splitAt n operators
                        do_calculate ((ys ++ suber:(tail (tail zs))), bs++(tail cs))
                Just n -> do
                    let multi = mul (digit!!n) (digit!!(n+1))
                    let (ys,zs) = splitAt n digit
                    let (bs,cs) = splitAt n operators
                    do_calculate ((ys ++ multi:(tail (tail zs))), bs++(tail cs))
            Just n -> do
                let diviser = divd (digit!!n) (digit!!(n+1))
                let (ys,zs) = splitAt n digit
                let (bs,cs) = splitAt n operators
                do_calculate ((ys ++ diviser:(tail (tail zs))), bs++(tail cs))
        Just n -> do
            let powerer = pow (digit!!n) (digit!!(n+1))
            let (ys,zs) = splitAt n digit
            let (bs,cs) = splitAt n operators
            do_calculate ((ys ++ powerer:(tail (tail zs))), bs++(tail cs))
    _ -> print "Error"

calculate elem = do
    let word_tab = words elem
    let digi = []
    let oper = []
    case length word_tab of
        0 -> error "Invalid argument"
        1 -> print (word_tab !! 0)
        _ -> case ((length word_tab) `mod` 2) of
            1 -> do
                do_calculate (add_digi digi oper word_tab)
{-                print "Finished"-}
            _ -> error "Invalid calcul"
