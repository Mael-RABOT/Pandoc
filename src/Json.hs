{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- Json.hs
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Json (
        parseJson,
        JsonValue(..),
        formatJson) where

import Parse (
        Parser(..),
        parseString,
        parseSpaces,
        parseChar,
        parseInt,
        sepByChar
    )

import Formatter ( Formatter(..) )
import Types
    ( Item(..),
      Section(content, name),
      Paragraph(..),
      Text(Code, Normal, Italic, Bold),
      Links(..),
      Header(date, title, author) )

import Control.Applicative ( Alternative((<|>)) )
import Data.List (intercalate)

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


-- PRINTER JSON

formatJson :: Formatter
formatJson = Formatter
    { begin = "{\n"
    , onHeader = onHeaderJson
    , onBody = onBodyJson
    , end = (++ "\n}")}

onHeaderJson :: String -> Header -> String
onHeaderJson s h = s ++ "\"header\": {\n"
    ++ "\"title\": \"" ++ title h ++ "\""
    ++ maybe "" (\a -> ",\n\"author\": \"" ++ a ++ "\"" ) (author h)
    ++ maybe "" (\d -> ",\n\"date\": \"" ++ d ++ "\"" ) (date h)
    ++ "\n},\n"

onBodyJson :: String -> [Item] -> String
onBodyJson s items = s ++ "\"body\": [\n"  ++ forEachItem items ++ "\n]"

forEachItem :: [Item] -> String
forEachItem items = intercalate ",\n" (map toItemValue items)

toItemValue :: Item -> String
toItemValue item = case item of
        ParagraphItem para      -> paragraphToJson para
        ListItem list           -> listToJson list
        SectionItem sect        -> sectionToJson sect
        CodeBlockItem cblock    -> codeblockToJson cblock
        LinksItem links         -> linksToJson links

paragraphToJson :: Paragraph -> String
paragraphToJson para = case para of
    Content cont -> "[\n" ++ forEachItem cont ++ "\n]"
    Text txt     -> case txt of
        Normal str  -> show str
        Italic str  -> "{\n\"italic\": " ++ show str ++ "\n}"
        Bold str    -> "{\n\"bold\": " ++ show str ++ "\n}"
        Code str    -> "{\n\"code\": " ++ show str ++ "\n}"

listToJson :: [Item] -> String
listToJson list = "{\n\"list\": [\n"
    ++ forEachItem list
    ++ "\n]\n}"

sectionToJson :: Section -> String
sectionToJson sect = "{\n\"section\": {\n"
    ++ maybe "" (\t -> "\"title\": \"" ++ t ++ "\",\n" ) (name sect)
    ++ "\"content\": [\n"
    ++ forEachItem (content sect)
    ++ "\n]\n}\n}"

linksToJson :: Links -> String
linksToJson links = case links of
    Link url cont   -> "{\n\"link\":{\n\"url\":\"" ++ url
        ++ "\",\n\"content\": [\n" ++ forEachItem cont
        ++ "\n]\n}\n}"
    Image url alt   -> "{\n\"image\":{\n\"url\":\"" ++ url
        ++ "\",\n\"alt\": [\n" ++ forEachItem alt
        ++ "\n]\n}\n}"

codeblockToJson :: [Item] -> String
codeblockToJson cblock = "{\n\"codeblock\": [\n"
    ++ forEachItem cblock
    ++ "\n]\n}"
