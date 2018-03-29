module Translater
    (
        Interpretation(..),
        translater
    ) where

import Parser

data Interpretation = Addition Operator Interpretation Interpretation
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


term :: [Item] -> (Interpretation, [Item])
term t = let (leftTree, rest) = factor t in
            case rest of 
              ((TOperator op):t) | elem op [Times, Div] -> let (rightTree, rest') = term t in ((ProdNode op leftTree rightTree), rest')
              otherwise                                 -> (leftTree, rest)
-}

term :: [Item] -> (Interpretation, [Item])
term ((MyNumber nb):t) = ((Number (fromIntegral nb)), t)

expression :: [Item] -> (Interpretation, [Item])
expression t = let (leftTree, rest) = term t in
                case rest of
                    ((MyOperator op):t) | elem op [Add, Subs] -> let (rightTree, rest') = expression t in ((Addition op leftTree rightTree), rest')
                    _ -> (leftTree, rest)

translater :: [Item] -> Interpretation
translater tuple = let (tree, tokens) = expression tuple in
                    if null tokens
                        then tree
                    else error $ "Syntax Error: " ++ show tokens