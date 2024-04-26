{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- Xml.hs
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $>" #-}
{-# HLINT ignore "Use <&>" #-}

module Xml (parseXml, XmlTag(..), TagValue(..), formatXml ) where

import Formatter ( Formatter(..) )
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
import Types
    ( Item(..),
      Section(content, name),
      Paragraph(..),
      Text(Code, Normal, Italic, Bold),
      Links(..),
      Header(date, title, author) )

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

-- PRINTER XML

formatXml :: Formatter
formatXml = Formatter
    { begin = "<document>\n"
    , onHeader = onHeadetXml
    , onBody = onBodtXml
    , end = (++ "</document>")}

onHeadetXml :: String -> Header -> String
onHeadetXml s h = s ++ "<header title=\"" ++ title h ++ "\">\n"
    ++ maybe "" (\a -> "<author>" ++ a ++ "</author>\n" ) (author h)
    ++ maybe "" (\d -> "<date>" ++ d ++ "</date>\n" ) (date h)
    ++ "</header>\n"

onBodtXml :: String -> [Item] -> String
onBodtXml s items = s ++ "<body>"  ++ forEachItem items ++ "</body>\n"

forEachItem :: [Item] -> String
forEachItem = concatMap toItemValue

toItemValue :: Item -> String
toItemValue item = case item of
        ParagraphItem para      -> paragraphToXml para
        ListItem list           -> listToXml list
        SectionItem sect        -> sectionToXml sect
        CodeBlockItem cblock    -> codeblockToXml cblock
        LinksItem links         -> linksToXml links

paragraphToXml :: Paragraph -> String
paragraphToXml para = case para of
    Content cont -> "<paragraph>" ++ forEachItem cont ++ "</paragraph>"
    Text txt     -> case txt of
        Normal str  -> str
        Italic str  -> "<italic>" ++ str ++ "</italic>"
        Bold str    -> "<bold>" ++ str ++ "</bold>"
        Code str    -> "<code>" ++ str ++ "</code>"

listToXml :: [Item] -> String
listToXml list = "<list>"
    ++ forEachItem list
    ++ "</list>"

sectionToXml :: Section -> String
sectionToXml sect = "<section title=\"" ++ maybe "" id (name sect) ++ "\">"
    ++ forEachItem (content sect)
    ++ "</section>"

linksToXml :: Links -> String
linksToXml links = case links of
    Link url cont   -> "<link url=\"" ++ url ++ "\">"
        ++ forEachItem cont
        ++ "</link>"
    Image url alt   -> "<image url=\"" ++ url ++ "\">"
        ++ forEachItem alt
        ++ "</image>"

codeblockToXml :: [Item] -> String
codeblockToXml cblock = "<codeblock>"
    ++ forEachItem cblock
    ++ "</codeblock>"
