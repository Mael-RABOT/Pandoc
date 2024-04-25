{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- Xml.hs
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $>" #-}
{-# HLINT ignore "Use <&>" #-}

module Xml (parseXml, XmlTag(..), TagValue(..)) where

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
    dataTitle :: String,
    dataContent :: [TagValue],
    attributes :: [(String, String)]
} deriving (Show, Eq)

data TagValue =
    Tag XmlTag
    | XmlText String
    deriving (Show, Eq)

parseTagName :: Parser String
parseTagName = parseSome (parseSatisfy isAlphaNum)

parseText :: Parser String
parseText = parseSome (parseSatisfy isAlphaNum)

parseClosingTag :: String -> Parser XmlTag
parseClosingTag tagName = do
    _ <- parseChar '<'
    _ <- parseChar '/'
    closingTagName <- parseTagName
    if closingTagName == tagName
        then
            parseChar '>' *>
            pure (XmlTag closingTagName [XmlText ""] [])
        else
            Parser $ \_ -> Left "Mismatched closing XmlTag"

parseTagText :: Parser TagValue
parseTagText = parseSome (parseSatisfy (/= '<')) >>= (pure . XmlText)


parseTagValue :: Parser TagValue
parseTagValue = parseTag <|> parseTagText

parseAttribute :: Parser (String, String)
parseAttribute = do
    key <- parseText
    _ <- parseSpaces
    _ <- parseChar '='
    _ <- parseSpaces
    value <- parseString
    pure (key, value)

filterEmpty :: [TagValue] -> [TagValue]
filterEmpty = filter (not . isEmpty)
    where
        isEmpty :: TagValue -> Bool
        isEmpty (Tag _) = False
        isEmpty (XmlText v) = all isSpace v

parseTag :: Parser TagValue
parseTag = do
    _ <- parseChar '<'
    tagName <- parseTagName
    attrs <- parseSome (parseSpaces *> parseAttribute) <|> pure []
    _ <- parseChar '>'
    tagContent <- parseSome parseTagValue <|> pure []
    _ <- parseClosingTag tagName
    pure (Tag (XmlTag tagName (filterEmpty tagContent) attrs))

parseXml :: Parser TagValue
parseXml = parseTag
