{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- Json.hs
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Json (parseJson, JsonValue(..)) where

import Parse (
        Parser(..),
        parseString,
        parseSpaces,
        parseChar,
        parseInt,
        sepByChar
    )

import Control.Applicative ( Alternative((<|>)) )

data JsonValue
    = JsonNull
    | JsonBool Bool
    | JsonNumber Int
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving (Show, Eq)

parseJsonValue :: Parser JsonValue
parseJsonValue =
    parseJsonNull
    <|> parseJsonBool
    <|> parseJsonNumber
    <|> parseJsonString
    <|> parseJsonArray
    <|> parseJsonDict

parseJsonNull :: Parser JsonValue
parseJsonNull = Parser $ \input ->
    case input of
        "null"  -> Right (JsonNull, "")
        _       -> Left "Expected 'null'"

parseJsonBool :: Parser JsonValue
parseJsonBool = Parser $ \input ->
    case input of
        "true"  -> Right (JsonBool True, "")
        "false" -> Right (JsonBool False, "")
        _       -> Left "Expected 'true' or 'false'"

parseJsonNumber :: Parser JsonValue
parseJsonNumber = Parser $ \input ->
    case runParser parseInt input of
        Right (res, remain) -> Right (JsonNumber res, remain)
        Left err -> Left err

parseJsonString :: Parser JsonValue
parseJsonString = fmap JsonString parseString

parseJsonArray :: Parser JsonValue
parseJsonArray = do
    _ <- parseChar '['
    _ <- parseSpaces
    values <- parseJsonValue `sepByChar` ','
    _ <- parseSpaces
    _ <- parseChar ']'
    return (JsonArray values)

parseJsonDict :: Parser JsonValue
parseJsonDict = do
    _ <- parseChar '{'
    _ <- parseSpaces
    pairs <- parseJsonObject `sepByChar` ','
    _ <- parseSpaces
    _ <- parseChar '}'
    return (JsonObject pairs)

parseJsonObject :: Parser (String, JsonValue)
parseJsonObject = do
    key <- parseString
    _ <- parseSpaces
    _ <- parseChar ':'
    _ <- parseSpaces
    value <- parseJsonValue
    return (key, value)

parseJson :: Parser JsonValue
parseJson = parseJsonValue
