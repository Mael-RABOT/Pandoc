{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- Parse.hs
-}

module Parse (parseChar, parseInt, parseMany, parseSome,
    parseSpaces, sepByChar, parseString, Parser(..), parseSatisfy) where

import Control.Applicative
import Text.Read (readMaybe)
import Data.Char (isSpace)

data Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

instance Functor Parser where
    fmap fct parser = Parser $ \ input ->
        case runParser parser input of
            Right (res, rem) -> Right (fct res, rem)
            Left err -> Left err

instance Applicative Parser where
    pure x = Parser $ \ input -> Right (x, input)
    (Parser p1) <*> (Parser p2) = Parser $ \input ->
        case p1 input of
            Right (f, rem) -> case p2 rem of
                Right (res, rem2) -> Right (f res, rem2)
                Left err -> Left err
            Left err -> Left err

instance Alternative Parser where
    empty = Parser $ \ input -> Left "Empty"
    (Parser p1) <|> (Parser p2) = Parser $ \input ->
        case p1 input of
            Right res -> Right res
            Left _ -> p2 input

instance Monad Parser where
    (Parser p) >>= f = Parser $ \input ->
        case p input of
            Right (result, remaining) -> runParser (f result) remaining
            Left err -> Left err

parseChar :: Char -> Parser Char
parseChar c = Parser $ \input ->
    case input of
        (x:xs)
            | c == x -> Right (c, xs)
            | otherwise -> Left $ "Expected '" ++ [c] ++
            "', found '" ++ [x] ++ "'"
        _ -> Left "Unexpected end of input"

parseInt :: Parser Int
parseInt = Parser $ \input ->
    case span (`elem` ('-':['0'..'9'])) input of
        (digits, rest)
            | null digits -> Left "Expected digits"
            | otherwise ->
                case readMaybe digits of
                    Just n -> Right (n, rest)
                    Nothing -> Left "Invalid number format"

parseMany :: Parser a -> Parser [a]
parseMany p = Parser $ \input ->
    let collectResults acc remaining =
            case runParser p remaining of
                Right (result, remaining') ->
                    collectResults (acc ++ [result]) remaining'
                Left _ -> Right (acc, remaining)
    in collectResults [] input

parseSome :: Parser a -> Parser [a]
parseSome p = do
    first <- p
    rest <- parseMany p
    return (first : rest)

parseSpaces :: Parser String
parseSpaces = Parser $ \input ->
    let keep = dropWhile isSpace input
    in Right ("", keep)

sepByChar :: Parser a -> Char -> Parser [a]
sepByChar p delim = do
    parseSpaces
    let collectValues = do
            first <- p
            parseSpaces
            rest <- parseMany (parseChar delim *> parseSpaces *> p)
            return (first : rest)
    collectValues <|> pure []

parseString :: Parser String
parseString = Parser $ \input ->
    case input of
        ('"':rest) -> case span (/= '"') rest of
            (str, ('"':xs)) -> Right (str, xs)
            _ -> Left "Unterminated string"
        _ -> Left "Expected string"

parseSatisfy :: (Char -> Bool) -> Parser Char
parseSatisfy f = Parser $ \input ->
    case input of
        (x:xs)
            | f x -> Right (x, xs)
            | otherwise -> Left "Parse error: satisfying predicate failed"
        _ -> Left "Parse error: unexpected end of input"
