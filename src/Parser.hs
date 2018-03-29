module Parser
    (
        parser,
        Operator(..),
        Item(..),
    ) where

import Data.Char

data Operator = Add
            | Subs
            deriving (Show, Eq)

data Item = MyOperator Operator
        | MyNumber Int
        deriving (Show, Eq)

operator :: Char -> Operator
operator c = case c of
            '+' -> Add
            _ -> Subs

parser :: String -> [Item]
parser [] = []
parser (h:t) | elem h "+-" = MyOperator (operator h) : parser t
             | isDigit h = let (numbers, rest) = span isDigit (h:t) in
                                MyNumber (read numbers) : parser rest
             | isSpace h = parser t
             | otherwise = error $ "Bad tokenizer input \"" ++ [h] ++ "\""
