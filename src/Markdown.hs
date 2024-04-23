{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- Markdown.hs
-}

module Markdown (parseMarkdown) where

import Control.Applicative
import Parse (Parser(..), parseSome, parseSpaces, parseChar, parseSatisfy)
import Data.List (isPrefixOf)

data Block =
    Paragraph [Inline]
    | Section String [Block]
    | List [Inline]
    deriving (Show)

data TextData =
    Normal String
    | Italic String
    | Bold String
    | Code String
    deriving (Show)

data Inline =
    Text TextData
    | Link String String
    | CodeBlock String
    deriving (Show)

data Markdown = Markdown {
    header :: [(String, String)],
    body :: [Block]
} deriving (Show)

parsePrefix :: String -> Parser String
parsePrefix pre = Parser $ \ input ->
    if isPrefixOf pre input then Right (pre, drop (length pre) input)
    else Left $ "Prefix " ++ pre ++ " not found"

parseWhileChar :: Char -> Parser String
parseWhileChar c = parseSome (parseSatisfy (/=c))

parseMarkdownHeaderData :: Parser (String, String)
parseMarkdownHeaderData =
    parseSatisfy (/='-') >>= \c ->
    parseSpaces >>
    parseWhileChar ':' >>= \key ->
    parseChar ':' >>
    parseSpaces >>
    parseWhileChar '\n' >>= \value ->
    parseChar '\n' >>
    pure (c:key, value)

parseHeader :: Parser [(String, String)]
parseHeader =
    parsePrefix "---" >>
    parseChar '\n' >>
    parseSome parseMarkdownHeaderData >>= \ d ->
    parsePrefix "---" >>
    pure d

parseBase :: Parser String
parseBase = parseSome (parseSatisfy (not . (\c -> c `elem` "\n`*[]-#")))

parseTextDataNormal :: Parser TextData
parseTextDataNormal = parseBase >>= pure . Normal

parseTextDataItalic :: Parser TextData
parseTextDataItalic = (parseChar '*' *> parseBase <* (parseChar '*')) >>=
    pure . Italic

parseTextDataBold :: Parser TextData
parseTextDataBold = (parsePrefix "**" *> parseBase <* (parseChar '*') <* (parseChar '*')) >>=
    pure . Bold

parseTextDataCode :: Parser TextData
parseTextDataCode = (parseChar '`' *> parseBase <* (parseChar '`')) >>=
    pure . Code

parseTextData :: Parser TextData
parseTextData = parseTextDataCode <|> parseTextDataBold <|> parseTextDataItalic <|> parseTextDataNormal

parseList :: Parser Block
parseList = (parseSome (parseSpaces *> (parseChar '-') *>
    parseSpaces *> parseInline)) >>= (pure . List)

parseSection :: Parser Block
parseSection =
    parseSpaces >>
    parseSome (parseSatisfy (=='#')) >>
    parseSpaces >>
    parseBase >>= \ name ->
    parseSpaces >>
    parseBody >>= pure . (Section name)


parseLink :: Parser Inline
parseLink =
    parseChar '[' >>
    parseBase >>= \ name ->
    parseChar ']' >>
    parseSpaces >>
    parseChar '(' >>
    parseWhileChar ')' >>= \ value ->
    parseChar ')' >>
    pure (Link name value)

parseCodeBlock :: Parser Inline
parseCodeBlock = (parsePrefix "```" *> parseSpaces *>
    parseBase <* parseSpaces <* ((parseChar '`') <* (parseChar '`')
    <* (parseChar '`'))) >>=
        pure . CodeBlock

parseInline :: Parser Inline
parseInline = parseLink <|> parseCodeBlock <|> (parseTextData >>= pure . Text)

parseParagraph :: Parser [Inline]
parseParagraph = parseSome (parseSpaces *> parseInline)

parseBlock :: Parser Block
parseBlock = parseSection <|> parseList <|> (parseParagraph >>= pure . Paragraph)
-- <|>

parseBody :: Parser [Block]
parseBody = parseSome (parseSpaces *> parseBlock)

parseMarkdown :: Parser Markdown
parseMarkdown = parseHeader >>= \ h ->
    parseBody >>= pure . (Markdown h)
