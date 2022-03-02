module Main where

import Lib
--import Data.Constraint.Forall

import Data.Char
import Data.Maybe
import Data.List
import Text.Printf

import System.Environment
import System.IO
import System.Exit

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

data Token = Operator Char |
             Number Float  |
             OpenPar       |
             Error         |
             ClosePar
             deriving (Show, Eq)

parseChar:: Char -> Parser Char
parseChar c = Parser $ \s -> case s of
    [] -> Nothing
    (x:xs) ->
        if x == c
        then Just (x, xs)
        else Nothing

parseAnyChar:: String -> Parser Char
parseAnyChar [] = Parser $ \s -> Nothing
parseAnyChar str = Parser $ \s -> case s of
    [] -> Nothing
    str1 ->
        if runParser (parseChar (head str)) str1 == Nothing
        then runParser (parseAnyChar (tail str)) (str1)
        else Just (head str, tail str1)

parseOr:: Eq a => Parser a -> Parser a -> Parser a
parseOr parser1 parser2 = Parser $ \s -> case runParser parser1 s of
    Nothing -> runParser parser2 s
    otherwise -> runParser parser1 s

parseAnd:: Eq a => Eq b => Parser a -> Parser b -> Parser (a, b)
parseAnd parser1 parser2 = Parser $ \s -> case runParser parser1 s of
    Nothing -> Nothing
    Just (first, s1) ->
        if runParser parser2 s1 == Nothing
        then Nothing
        else Just ((first, second), s2)
            where
                Just (second, s2) = runParser parser2 s1

parseAndWith :: Eq a => Eq b => Eq c => (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith function parser1 parser2 = Parser $ \s -> case runParser (parseAnd parser1 parser2) s of
    Nothing -> Nothing
    Just ((first, second), str1) -> Just ((function first second), str1)


applyIt:: Eq a => Parser a -> String -> [a]
applyIt parser [] = []
applyIt parser (x:xs) | runParser parser (x:xs) /= Nothing = [result] ++ applyIt parser (xs)
                      | otherwise = []
                      where
                          Just (result, str) = runParser parser (x:xs)

parseMany:: Eq a => Parser a -> Parser [a]
parseMany parser = Parser $ \s -> let result = applyIt parser s in
    Just (result, drop (length result) s)


parseSome:: Eq a => Parser a -> Parser [a]
parseSome parser = Parser $ \s -> case applyIt parser s of
    [] -> Nothing
    otherwise -> runParser (parseMany parser) s

parseInt:: Parser Int
parseInt = Parser $ \s -> case s of
     [] -> Nothing
     s ->
         if runParser (parseAnyChar (['0'..'9'])) s == Nothing
        then Nothing
        else Just (cvt, rest)
             where
                 Just (number, rest) = runParser (parseSome (parseAnyChar (['0'..'9']))) s
                 cvt = read number :: Int

parseFloat:: Parser Float
parseFloat = Parser $ \s -> case s of
     [] -> Nothing
     s ->
         if runParser (parseAnyChar (['0'..'9'] ++ ".")) s == Nothing
        then Nothing
        else Just (cvt, rest)
             where
                 Just (number, rest) = runParser (parseSome (parseAnyChar (['0'..'9'] ++ "."))) s
                 cvt = read number :: Float

getTuple:: Parser String
getTuple = Parser $ \s -> case s of
     [] -> Nothing
     s ->
         if runParser (parseAnyChar (['0'..'9'] ++ ".,()")) s == Nothing
        then Nothing
        else Just (tuple, rest)
             where
                Just (tuple, rest) = runParser (parseSome (parseAnyChar (['0'..'9'] ++ ".,()"))) s

checkTuple:: String -> Int
checkTuple [] = 0
checkTuple str | head str == '(' || head str == ')' || head str == ',' = number + 1
               | otherwise = checkTuple (tail str)
                where
                    number = checkTuple (tail str)

parseofTuples:: Eq a => Parser a -> Parser (a, a)
parseofTuples parser = Parser $ \s-> case runParser parser (tail s) of
    Nothing -> Nothing
    Just (first, s1) ->
        if runParser parser (tail s1) == Nothing
        then Nothing
        else Just ((first, second), s2)
        where
            Just (second, s2) = runParser parser (tail s1)

parseTuple:: Eq a => Parser a -> Parser (a, a)
parseTuple parser = Parser $ \s -> case runParser getTuple s of
    Nothing -> Nothing
    Just (tuple, rest) ->
        if (checkTuple tuple /= 3)
        then Nothing
        else Just (parsed, rest)
           where
               Just (parsed, s1) = runParser (parseofTuples parser) tuple

tokenize:: String -> [Token]
tokenize [] = []
tokenize str | head str == '(' = [OpenPar] ++ tokenize (tail str)
             | head str == ' ' = tokenize (tail str)
             | head str == ')' = [ClosePar] ++ tokenize (tail str)
             | head str `elem` ['+', '-', '/', '*', '^'] = [Operator (head str)] ++ tokenize (tail str)
             | runParser parseFloat str /= Nothing = [Number element] ++ tokenize s
             where
                 Just (element, s) = runParser parseFloat str
tokenize str = [Error]

cleanToken:: [Token] -> [Token]
cleanToken [] = []
cleanToken (Error:xs) = [Error]
cleanToken (x:[]) = [x]
cleanToken ((Number first):xs) = case head xs of
    Operator '-' -> [Number first] ++ [head xs] ++ cleanToken (tail xs)
    otherwise -> [Number first] ++ cleanToken xs
cleanToken ((Operator z):xs) = case z of
    '-' -> cleanToken ([Number (0 - second)] ++ rest)
        where
            Number second = head(xs)
            rest = (tail xs)
    otherwise -> [Operator z] ++ cleanToken xs
cleanToken (x:xs) = [x] ++ cleanToken xs

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

printNumber :: Token -> IO()
printNumber (Number n) = printf "%.2f\n" (n :: Float)


countCons :: [Token] -> Float
countCons (ClosePar:xs) = 0
countCons ((Number n):elements) = n + (countCons elements)
countCons elements = countCons (tail elements)

mathError :: [Token] -> Maybe [Token]
mathError [] = Just []
mathError (Error:xs) = Nothing
mathError ((Operator n):xs) = case n of
    '/' ->
        if head xs == (Number 0) 
        then Nothing
        else mathError xs
    otherwise -> mathError xs
mathError tokens = mathError (tail tokens)

error :: [Token] -> Maybe [Token]
error (OpenPar:tokens) | head tokens == ClosePar = Nothing

main :: IO ()
main = do
    args <- getArgs
    let tokens = cleanToken $ tokenize (head args)
    let ret = mathError tokens
    if ret /= Nothing && tokens /= [] && tokens /= [Error]
    then let result = evalExpr (evalExprMain tokens [] []) in 
        printNumber (head (snd result))
    else exitWith (ExitFailure 84)
