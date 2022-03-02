module Evaluation 
    (   precedence,
        applyOp,
        sumParenthesis,
        sumPrecedence,
        evalExpr,
        evalExprMain) where

import Token

precedence :: Token -> Int
precedence op | op == Operator '-' = 1
              | op == Operator '+' = 1
              | op == Operator '*' = 2
              | op == Operator '/' = 2
              | op == Operator '^' = 3
              | otherwise = 0

applyOp :: Token -> Token -> Token -> Token
applyOp (Number a) (Operator op) (Number b) | op == '+' = Number (a + b)
                                            | op == '-' = Number (a - b)
                                            | op == '*' = Number (a * b)
                                            | op == '/' = Number (a / b)
                                            | op == '^' = Number (a ** b)
                                            | otherwise = Number 0

sumParenthesis :: [Token] -> [Token] -> ([Token], [Token])
sumParenthesis [] qVals = ([], qVals)
sumParenthesis (OpenPar : qOps) qVals = (qOps, qVals)
sumParenthesis (op: qOps) (b : (a : qVals)) = sumParenthesis qOps (applyOp a op b : qVals)

sumPrecedence :: Token -> [Token] -> [Token] -> ([Token], [Token])
sumPrecedence thisOp [] qVals = ([thisOp], qVals)
sumPrecedence thisOp (op : qOps) (b : []) = (thisOp :op : qOps, [b])
sumPrecedence thisOp (op : qOps) (b : (a : qVals)) | precedence op >= precedence thisOp = sumPrecedence thisOp qOps (applyOp a op b : qVals)
                                                   | otherwise = (thisOp : op : qOps, b : a : qVals)

evalExprMain:: [Token] -> [Token] -> [Token] -> ([Token], [Token])
evalExprMain [] qOps qVals = (qOps, qVals)
evalExprMain ((Number n):xs) qOps qVals = evalExprMain xs qOps (Number n : qVals)
evalExprMain token qOps qVals | head token == OpenPar = evalExprMain (tail token) (head token : qOps) qVals
                              | head token == ClosePar = evalExprMain (tail token) n_qOps n_qVals
                              | otherwise = evalExprMain (tail token) new_qOps new_qVals
                        where
                            (n_qOps, n_qVals) = sumParenthesis qOps qVals
                            (new_qOps, new_qVals) = sumPrecedence (head token) qOps qVals

evalExpr:: ([Token] , [Token]) -> ([Token], [Token])
evalExpr ([], qVals) = ([], qVals)
evalExpr (op : qOps, b: a : qVals) = evalExpr (qOps, applyOp a op b : qVals)
