{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- Markdown.hs
-}

module Markdown (formatMarkdown) where

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

-- data Block =
--     Paragraph [Inline]
--     | Section String [Block]
--     | List [Inline]
--     deriving (Show)

-- data TextData =
--     Normal String
--     | Italic String
--     | Bold String
--     | Code String
--     deriving (Show)

-- data Inline =
--     Text TextData
--     | Link String String
--     | CodeBlock String
--     deriving (Show)

-- data Markdown = Markdown {
--     header :: [(String, String)],
--     body :: [Block]
-- } deriving (Show)

-- parsePrefix :: String -> Parser String
-- parsePrefix pre = Parser $ \ input ->
--     if isPrefixOf pre input then Right (pre, drop (length pre) input)
--     else Left $ "Prefix " ++ pre ++ " not found"

-- parseWhileChar :: Char -> Parser String
-- parseWhileChar c = parseSome (parseSatisfy (/=c))

-- parseMarkdownHeaderData :: Parser (String, String)
-- parseMarkdownHeaderData =
--     parseSatisfy (/='-') >>= \c ->
--     parseSpaces >>
--     parseWhileChar ':' >>= \key ->
--     parseChar ':' >>
--     parseSpaces >>
--     parseWhileChar '\n' >>= \value ->
--     parseChar '\n' >>
--     pure (c:key, value)

-- parseHeader :: Parser [(String, String)]
-- parseHeader =
--     parsePrefix "---" >>
--     parseChar '\n' >>
--     parseSome parseMarkdownHeaderData >>= \ d ->
--     parsePrefix "---" >>
--     pure d

-- parseBase :: Parser String
-- parseBase = parseSome (parseSatisfy (not . (\c -> c `elem` "\n`*[]-#")))

-- parseTextDataNormal :: Parser TextData
-- parseTextDataNormal = parseBase >>= pure . Normal

-- parseTextDataItalic :: Parser TextData
-- parseTextDataItalic = (parseChar '*' *> parseBase <* (parseChar '*')) >>=
--     pure . Italic

-- parseTextDataBold :: Parser TextData
-- parseTextDataBold = (parsePrefix "**" *> parseBase <* (parseChar '*')
--     <* (parseChar '*')) >>=
--     pure . Bold

-- parseTextDataCode :: Parser TextData
-- parseTextDataCode = (parseChar '`' *> parseBase <* (parseChar '`')) >>=
--     pure . Code

-- parseTextData :: Parser TextData
-- parseTextData =
--     parseTextDataCode
--     <|> parseTextDataBold
--     <|> parseTextDataItalic
--     <|> parseTextDataNormal

-- parseList :: Parser Block
-- parseList = (parseSome (parseSpaces *> (parseChar '-') *>
--     parseSpaces *> parseInline)) >>= (pure . List)

-- parseSection :: Parser Block
-- parseSection =
--     parseSpaces >>
--     parseSome (parseSatisfy (=='#')) >>
--     parseSpaces >>
--     parseBase >>= \ name ->
--     parseSpaces >>
--     parseBody >>= pure . (Section name)


-- parseLink :: Parser Inline
-- parseLink =
--     parseChar '[' >>
--     parseBase >>= \ name ->
--     parseChar ']' >>
--     parseSpaces >>
--     parseChar '(' >>
--     parseWhileChar ')' >>= \ value ->
--     parseChar ')' >>
--     pure (Link name value)

-- parseCodeBlock :: Parser Inline
-- parseCodeBlock = (parsePrefix "```" *> parseSpaces *>
--     parseBase <* parseSpaces <* ((parseChar '`') <* (parseChar '`')
--     <* (parseChar '`'))) >>=
--         pure . CodeBlock

-- parseInline :: Parser Inline
-- parseInline =
--     parseLink
--     <|> parseCodeBlock
--     <|> (parseTextData >>= pure . Text)

-- parseParagraph :: Parser [Inline]
-- parseParagraph = parseSome (parseSpaces *> parseInline)

-- parseBlock :: Parser Block
-- parseBlock =
--     parseSection
--     <|> parseList
--     <|> (parseParagraph >>= pure . Paragraph)

-- parseBody :: Parser [Block]
-- parseBody = parseSome (parseSpaces *> parseBlock)

-- parseMarkdown :: Parser Markdown
-- parseMarkdown = parseHeader >>= \ h ->
--     parseBody >>= pure . (Markdown h)

-- PRINTER MARKDOWN

formatMarkdown :: Formatter
formatMarkdown = Formatter
    { begin = ""
    , onHeader = onHeaderMarkdown
    , onBody = onBodyMarkdown
    , end = (++ "\n")}

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
forEachItem [x] i = "\n" ++ toItemValue x i
forEachItem (x:xs) i = "\n" ++ toItemValue x i ++ "\n"
    ++ forEachItem xs i

forEachItemInside :: [Item] -> Int -> String
forEachItemInside [] _ = ""
forEachItemInside (x:xs) i = toItemValue x i
    ++ forEachItemInside xs i

toItemValue :: Item -> Int -> String
toItemValue item indent = case item of
        ParagraphItem para      -> paragraphToMarkdown para indent
        ListItem list           -> listToMarkdown list indent
        SectionItem sect        -> sectionToMarkdown sect (indent + 1)
        CodeBlockItem cblock    -> codeblockToMarkdown cblock
        LinksItem links         -> linksToMarkdown links

paragraphToMarkdown :: Paragraph -> Int -> String
paragraphToMarkdown para indent = case para of
    Content cont -> forEachItemInside cont indent
    Text txt     -> case txt of
        Normal str  -> str
        Italic str  -> "*" ++ str ++ "*"
        Bold str    -> "**" ++ str ++ "**"
        Code str    -> "`" ++ str ++ "`"

listToMarkdown :: [Item] -> Int -> String
listToMarkdown [] _ = ""
listToMarkdown [x] i = "- " ++ toItemValue x i
listToMarkdown (x:xs) i = "- " ++ toItemValue x i ++ "\n"
    ++ listToMarkdown xs i

sectionToMarkdown :: Section -> Int -> String
sectionToMarkdown sect indent = case name sect of
    Nothing  -> ""
    Just ""  -> ""
    Just n   -> concat (replicate indent "#") ++ " " ++ n ++ "\n"
    ++ forEachItem (content sect) indent

linksToMarkdown :: Links -> String
linksToMarkdown links = case links of
    Link url cont   -> "[" ++ forEachItemInside cont 0 ++ "]("
        ++ url ++ ")"
    Image url alt   -> "[" ++ forEachItemInside alt 0 ++ "]("
        ++ url ++ ")"

codeblockToMarkdown :: [Item] -> String
codeblockToMarkdown cblock = "```\n"
    ++ forEachItemInside cblock 0
    ++ "\n```"
