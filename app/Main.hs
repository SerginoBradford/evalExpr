module Main where

import Lib
import Token
import Parser
import Evaluation

import Data.Char
import Data.Maybe
import Data.List
import Text.Printf

import System.Environment
import System.IO
import System.Exit

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
