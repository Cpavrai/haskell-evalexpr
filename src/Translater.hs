module Translater
    (
        Interpretation(..),
        translater
    ) where

import Parser

data Interpretation = Addition Operator Interpretation Interpretation
                    | Multiplication Operator Interpretation Interpretation
                    | Powering Operator Interpretation Interpretation
                    | Unary Operator Interpretation
                    | Number Double
                    deriving (Show)

power :: [Item] -> (Interpretation, [Item])
power ((MyNumber nb):t) = ((Number (fromIntegral nb)), t)
power ((MyOperator op):t) | elem op [Add, Subs] = let (tree , rest) = power t in
                              ((Unary op tree), rest)
                          | op == ParB = let (tree, rest) = expression t in
                                case rest of
                                    ((MyOperator op):rest) | op == ParE -> (tree, rest)
                                    otherwise -> error $ "Mismatching parenthesis"
power t | otherwise = error $ "Empty Tokens"

factor :: [Item] -> (Interpretation, [Item])
factor ((MyOperator operator_one):t) | operator_one == Root = let (tree, rest) = factor t in ((Powering operator_one tree (Number 0.5)), rest)
factor t = let (leftTree, rest) = power t in
            case rest of 
                ((MyOperator operator_one):t) | operator_one == Pow -> let (rightTree, rest') = factor t in ((Powering operator_one leftTree rightTree), rest')
                _ -> (leftTree, rest)



term :: [Item] -> (Interpretation, [Item])
term t = let (leftTree, rest) = factor t in
        case rest of
            ((MyOperator operator_one):t) | elem operator_one [Mul, Div] -> let (rightTree, rest') = term t in
                ((Multiplication operator_one leftTree rightTree), rest')
            _ -> (leftTree, rest)


expression :: [Item] -> (Interpretation, [Item])
expression t = let (leftTree, rest) = term t in
                case rest of
                    ((MyOperator operator_one):t) | elem operator_one [Add, Subs] -> let (rightTree, rest') = expression t in
                        ((Addition operator_one leftTree rightTree), rest')
                    _ -> (leftTree, rest)

translater :: [Item] -> Interpretation
translater tuple = let (tree, tokens) = expression tuple in
                    if null tokens
                        then tree
                    else error $ "Syntax Error: " ++ show tokens