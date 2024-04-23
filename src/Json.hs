{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- Json.hs
-}

module Json (parseJson, JsonValue(..)) where

import Parse (
        Parser(..),
        parseString,
        parseSpaces,
        parseChar,
        parseInt,
        sepByChar
    )

import Control.Applicative

type JsonItem = (String, JsonValue)

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
        "null" -> Right (JsonNull, "")
        _ -> Left "Expected 'null'"

parseJsonBool :: Parser JsonValue
parseJsonBool = Parser $ \input ->
    case input of
        "true" -> Right (JsonBool True, "")
        "false" -> Right (JsonBool False, "")
        _ -> Left "Expected 'true' or 'false'"

parseJsonNumber :: Parser JsonValue
parseJsonNumber = Parser $ \input ->
    case runParser parseInt input of
        Right (res, rem) -> Right (JsonNumber res, rem)
        Left err -> Left err

parseJsonString :: Parser JsonValue
parseJsonString = fmap JsonString parseString

parseJsonArray :: Parser JsonValue
parseJsonArray = do
    parseChar '['
    parseSpaces
    values <- parseJsonValue `sepByChar` ','
    parseSpaces
    parseChar ']'
    return (JsonArray values)

parseJsonDict :: Parser JsonValue
parseJsonDict = do
    parseChar '{'
    parseSpaces
    pairs <- parseJsonObject `sepByChar` ','
    parseSpaces
    parseChar '}'
    return (JsonObject pairs)

parseJsonObject :: Parser (String, JsonValue)
parseJsonObject = do
    key <- parseString
    parseSpaces
    _ <- parseChar ':'
    parseSpaces
    value <- parseJsonValue
    return (key, value)

parseJson :: Parser JsonValue
parseJson = parseJsonValue
