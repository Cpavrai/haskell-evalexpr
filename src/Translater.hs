module Translater
    (
        Interpretation(..),
        translater
    ) where

import Parser

data Interpretation = Addition Operator Interpretation Interpretation
                    | Multiplication Operator Interpretation Interpretation
                    | Number Double
                    deriving (Show)

{-power :: [Item] -> (Interpretation, [Item])
power ((TNumber nb):t) = ((NumNode (fromIntegral nb)), t)
power ((TOperator op):t) | elem op [Plus, Minus] = let (tree , rest) = power t in ((UnaryNode op tree), rest)
                         | op == PLeft = let (tree, rest) = expression t in
                                            case rest of
                                              ((TOperator op):rest) | op == PRight -> (tree, rest)
                                              otherwise -> error $ "Mismatching parenthesis"
power t | otherwise = error $ "Empty Tokens"

factor :: [Item] -> (Interpretation, [Item])
factor ((TOperator op):t) | op == Root = let (tree, rest) = factor t in ((PowNode op tree (NumNode 0.5)), rest)
factor t = let (leftTree, rest) = power t in
              case rest of 
                ((TOperator op):t) | op == Power -> let (rightTree, rest') = factor t in ((PowNode op leftTree rightTree), rest')
                otherwise                        -> (leftTree, rest)


-}
factor :: [Item] -> (Interpretation, [Item])
factor ((MyNumber nb):t) = ((Number (fromIntegral nb)), t)

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