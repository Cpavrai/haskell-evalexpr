module Translater
    (
        Interpretation(..),
        translater
    ) where

import Parser

data Interpretation = Addition Operator Interpretation Interpretation
                    | Multiplication Operator Interpretation Interpretation
                    | Powering Operator Interpretation Interpretation
                    | Ending Operator Interpretation
                    | Number Double
                    deriving (Show)

first :: [Item] -> (Interpretation, [Item])
first ((MyNumber nb):t) = ((Number (fromIntegral nb)), t)

first ((MyOperator op):t) | elem op [Add, Subs] = let (tree , rest) = first t in
                              ((Ending op tree), rest)
                          | op == ParB = let (tree, rest) = expression t in
                                case rest of
                                    ((MyOperator op):rest) | op == ParE -> (tree, rest)
                                    otherwise -> error ("Parenthesis calcul end not detected")


second :: [Item] -> (Interpretation, [Item])
second ((MyOperator operator_one):t) | operator_one == Root = let (tree, rest) = second t in
                                                                ((Powering operator_one tree (Number 0.5)), rest)

second t = let (tree, rest) = first t in
            case rest of 
                ((MyOperator operator_one):t) | operator_one == Pow -> let (tree', rest') = second t in
                                                                        ((Powering operator_one tree tree'), rest')
                _ -> (tree, rest)


third :: [Item] -> (Interpretation, [Item])
third t = let (tree, rest) = second t in
        case rest of
            ((MyOperator operator_one):t) | elem operator_one [Mul, Div] -> let (tree', rest') = third t in
                ((Multiplication operator_one tree tree'), rest')
            _ -> (tree, rest)


expression :: [Item] -> (Interpretation, [Item])
expression t = let (tree, rest) = third t in
                case rest of
                    ((MyOperator operator_one):t) | elem operator_one [Add, Subs] -> let (tree', rest') = expression t in
                        ((Addition operator_one tree tree'), rest')
                    _ -> (tree, rest)


translater :: [Item] -> Interpretation
translater tuple = let (tree, tokens) = expression tuple in
                    case tokens of
                        null -> tree
                        _ -> error ("Syntax Error")
