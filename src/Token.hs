module Token 
        (Token(..),
        tokenize,
        cleanToken) where

import Parser

data Token = Operator Char |
             Number Float  |
             OpenPar       |
             Error         |
             ClosePar
             deriving (Show, Eq)

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
