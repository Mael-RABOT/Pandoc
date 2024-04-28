{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- Markdown.hs
-}

module Markdown (formatMarkdown, parseMarkdown, Markdown(..), Block(..), TextData(..), Inline(..)) where

import Formatter ( Formatter(..) )
import Control.Applicative
import Parse (Parser(..), parseSome, parseSpaces, parseChar, parseSatisfy)
import Data.List (isPrefixOf, intercalate)
import Types
    ( Item(..),
      Section(content, name),
      Paragraph(..),
      Text(Code, Normal, Italic, Bold),
      Links(..),
      Header(date, title, author) )
import GHC.IO.Exception (cannotCompactFunction)

data Block =
    MdParagraph [Inline]
    | MdSection String [Block]
    | MdList [Inline]
    deriving (Show)

data TextData =
    TextNormal String
    | TextItalic String
    | TextBold String
    | TextCode String
    deriving (Show)

data Inline =
    InlineText TextData
    | InlineLink String String
    | InlineCodeBlock String
    deriving (Show)

data Markdown = Markdown {
    mdHeader :: [(String, String)],
    mdBody :: [Block]
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
parseTextDataNormal = parseBase >>= pure . TextNormal

parseTextDataItalic :: Parser TextData
parseTextDataItalic = (parseChar '*' *> parseBase <* (parseChar '*')) >>=
    pure . TextItalic

parseTextDataBold :: Parser TextData
parseTextDataBold = (parsePrefix "**" *> parseBase <* (parseChar '*')
    <* (parseChar '*')) >>=
    pure . TextBold

parseTextDataCode :: Parser TextData
parseTextDataCode = (parseChar '`' *> parseBase <* (parseChar '`')) >>=
    pure . TextCode

parseTextData :: Parser TextData
parseTextData =
    parseTextDataCode
    <|> parseTextDataBold
    <|> parseTextDataItalic
    <|> parseTextDataNormal

parseList :: Parser Block
parseList = (parseSome (parseSpaces *> (parseChar '-') *>
    parseSpaces *> parseInline)) >>= (pure . MdList)

parseSectionContent :: Int -> Parser [Block]
parseSectionContent i = parseSome (parseSpaces *> (parseChildBlock i))

parseChildSection :: Int -> Parser Block
parseChildSection i =
    parseSpaces >>
    parsePrefix (replicate (i + 1) '#') >>
    parseSpaces >>
    parseBase >>= \ name ->
    parseSpaces >>
    (parseSectionContent i) >>= pure . (MdSection name)

parseSection :: Parser Block
parseSection =
    parseSpaces >>
    parseSome (parseSatisfy (=='#')) >>
    parseSpaces >>
    parseBase >>= \ name ->
    parseSpaces >>
    (parseSectionContent 1) >>= pure . (MdSection name)


parseLink :: Parser Inline
parseLink =
    parseChar '[' >>
    parseBase >>= \ name ->
    parseChar ']' >>
    parseSpaces >>
    parseChar '(' >>
    parseWhileChar ')' >>= \ value ->
    parseChar ')' >>
    pure (InlineLink name value)

parseCodeBlock :: Parser Inline
parseCodeBlock = (parsePrefix "```" *> parseSpaces *>
    parseBase <* parseSpaces <* ((parseChar '`') <* (parseChar '`')
    <* (parseChar '`'))) >>=
        pure . InlineCodeBlock

parseInline :: Parser Inline
parseInline =
    parseLink
    <|> parseCodeBlock
    <|> (parseTextData >>= pure . InlineText)

parseParagraph :: Parser [Inline]
parseParagraph = parseSome (parseSpaces *> parseInline)

parseBlock :: Parser Block
parseBlock =
    parseSection
    <|> parseList
    <|> (parseParagraph >>= pure . MdParagraph)
    -- <|> pure (Paragraph [])

parseChildBlock :: Int -> Parser Block
parseChildBlock i =
    (parseChildSection i)
    <|> parseList
    <|> (parseParagraph >>= pure . MdParagraph)
    -- c

parseBody :: Parser [Block]
parseBody = parseSome (parseSpaces *> parseBlock)

parseMarkdown :: Parser Markdown
parseMarkdown = parseHeader >>= \ h ->
    parseBody >>= pure . (Markdown h)

-- PRINTER MARKDOWN

formatMarkdown :: Formatter
formatMarkdown = Formatter
    { begin = ""
    , onHeader = onHeaderMarkdown
    , onBody = onBodyMarkdown
    , end = (++ "")}

onHeaderMarkdown :: String -> Header -> String
onHeaderMarkdown s h = s ++ "---\n"
    ++ "title: " ++ title h
    ++ maybe "" ("\nauthor: " ++) (author h)
    ++ maybe "" ("\ndate: " ++) (date h)
    ++ "\n---\n"

onBodyMarkdown :: String -> [Item] -> String
onBodyMarkdown s items = s ++ forEachItem items 0

forEachItem :: [Item] -> Int -> String
forEachItem [] _ = ""
forEachItem (x:xs) i = toItemValue x i
    ++ forEachItem xs i

toItemValue :: Item -> Int -> String
toItemValue item indent = case item of
        ParagraphItem para      -> paragraphToMarkdown para indent
        ListItem list           -> listToMarkdown list indent
        SectionItem sect        -> sectionToMarkdown sect (indent + 1)
        CodeBlockItem cblock    -> codeblockToMarkdown cblock
        LinksItem links         -> linksToMarkdown links

paragraphToMarkdown :: Paragraph -> Int -> String
paragraphToMarkdown para indent = case para of
    Content cont -> "\n" ++ forEachItem cont indent ++ "\n"
    Text txt     -> case txt of
        Normal str  -> str
        Italic str  -> "*" ++ str ++ "*"
        Bold str    -> "**" ++ str ++ "**"
        Code str    -> "`" ++ str ++ "`"

listToMarkdown :: [Item] -> Int -> String
listToMarkdown [] _ = ""
listToMarkdown (x:xs) i = "- " ++ toItemValue x i
    ++ "\n- " ++ listToMarkdown xs i

sectionToMarkdown :: Section -> Int -> String
sectionToMarkdown sect indent = case name sect of
    Nothing  -> ""
    Just ""  -> ""
    Just n   -> "\n" ++ concat (replicate indent "#") ++ " " ++ n ++ "\n"
    ++ forEachItem (content sect) indent

linksToMarkdown :: Links -> String
linksToMarkdown links = case links of
    Link url cont   -> "["++ forEachItem cont 0 ++ "]("
        ++ url ++ ")"
    Image url alt   -> "["++ forEachItem alt 0 ++ "]("
        ++ url ++ ")"

codeblockToMarkdown :: [Item] -> String
codeblockToMarkdown cblock = "\n```\n"
    ++ forEachItem cblock 0
    ++ "\n```\n"
