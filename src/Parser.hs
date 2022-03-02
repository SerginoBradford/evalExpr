module Parser 
    (   Parser(..),
        parseAnd,
        parseAndWith,
        parseAnyChar,
        parseChar,
        parseFloat,
        parseInt,
        parseMany,
        parseSome,
        parseTuple,
        parseOr) where

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

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
