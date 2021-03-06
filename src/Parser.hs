module Parser
    (
        parser,
        Operator(..),
        Item(..),
    ) where

import Data.Char

data Operator = Add
            | Subs
            | Mul
            | Div
            | Pow
            | Root
            | ParB
            | ParE
            deriving (Show, Eq)

data Item = MyOperator Operator
        | MyNumber Int
        deriving (Show, Eq)

translate_operator :: Char -> Operator
translate_operator c = case c of
            '+' -> Add
            '-' -> Subs
            '*' -> Mul
            '/' -> Div
            '^' -> Pow
            'v' -> Root
            '(' -> ParB
            ')' -> ParE
            _ -> error ("Unknown operator")

parser :: String -> [Item]
parser [] = []
parser (h:t) | elem h "+-*/^v()" = MyOperator (translate_operator h) : parser t
             | isDigit h = let (numbers, rest) = span isDigit (h:t) in
                                MyNumber (read numbers) : parser rest
             | isSpace h = parser t
             | otherwise = error ("Syntax not respected")
