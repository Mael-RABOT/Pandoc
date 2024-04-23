{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- Xml.hs
-}

module Xml (parseXml) where

import Data.Char (isAlphaNum, isSpace)
import Control.Applicative
import Parse (
        Parser(..),
        parseString,
        parseSpaces,
        parseChar,
        parseSome,
        parseSatisfy
    )


data XmlTag = XmlTag {
    name :: String,
    content :: [TagValue],
    attributes :: [(String, String)]
} deriving (Show, Eq)

data TagValue =
    Tag XmlTag
    | Text String
    deriving (Show, Eq)

parseTagName :: Parser String
parseTagName = parseSome (parseSatisfy isAlphaNum)

parseText :: Parser String
parseText = do
    text <- parseSome (parseSatisfy isAlphaNum)
    pure (text)

parseClosingTag :: String -> Parser XmlTag
parseClosingTag tagName = do
    parseChar '<'
    parseChar '/'
    closingTagName <- parseTagName
    if closingTagName == tagName
        then
            parseChar '>' *>
            pure (XmlTag closingTagName [(Text "")] [])
        else
            Parser $ \_ -> Left "Mismatched closing XmlTag"

parseTagText :: Parser TagValue
parseTagText = parseSome (parseSatisfy (\c -> c /= '<')) >>=
    (\x -> pure (Text x))


parseTagValue :: Parser TagValue
parseTagValue = parseTag <|> parseTagText

parseAttribute :: Parser (String, String)
parseAttribute = do
    key <- parseText
    parseSpaces
    parseChar '='
    parseSpaces
    value <- parseString
    pure (key, value)

filterEmpty :: [TagValue] -> [TagValue]
filterEmpty origin = filter (not . isEmpty) origin
    where
        isEmpty :: TagValue -> Bool
        isEmpty (Tag _) = False
        isEmpty (Text v) = all isSpace v

parseTag :: Parser TagValue
parseTag = do
    parseChar '<'
    tagName <- parseTagName
    attrs <- ((parseSome (parseSpaces *> parseAttribute)) <|> pure [])
    parseChar '>'
    tagContent <- ((parseSome parseTagValue) <|> pure [])
    parseClosingTag tagName
    pure (Tag (XmlTag tagName (filterEmpty tagContent) attrs))

parseXml :: Parser TagValue
parseXml = parseTag
