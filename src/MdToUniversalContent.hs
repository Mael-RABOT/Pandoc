{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- mdToUniversalContent.hs
-}

module MdToUniversalContent (markdownToUniversalContent) where

import Markdown (Markdown(..), parseMarkdown, Block(..), TextData(..), Inline(..))
import Types (Header(..), Item(..), Paragraph(..), UniversalContent(..), Section(..), Links(..), Text(..))
import Data.Maybe (mapMaybe)

findByKey :: [(String, String)] -> String -> Maybe String
findByKey ((k, v):xs) tc | tc == k = Just v
                            | otherwise = findByKey xs tc

markdownToUniversalContent :: Either String Markdown -> Either String UniversalContent
markdownToUniversalContent (Left err) = Left err
markdownToUniversalContent (Right md) = case convertHeader (mdHeader md) of
    Left err -> Left err
    Right h -> case convertBody (mdBody md) of
        Left err -> Left err
        Right b -> Right $ UniversalContent h b

convertHeader :: [(String, String)] -> Either String Header
convertHeader map = case findByKey map "title" of
    Nothing -> Left "Header's title field not found"
    Just t -> Right $ Header t (findByKey map "author") (findByKey map "date")

convertBody :: [Block] -> Either String [Item]
convertBody blocks = Right $ map blockToItem blocks

textDataToText :: TextData -> Text
textDataToText (TextNormal txt) = Normal txt
textDataToText (TextItalic txt) = Italic txt
textDataToText (TextBold txt) = Bold txt
textDataToText (TextCode txt) = Code txt

inlineToParagraph :: Inline -> Item
inlineToParagraph (InlineText t) = ParagraphItem $ Text (textDataToText t)
inlineToParagraph (InlineLink c l) = LinksItem $ Link l
    [ParagraphItem $ Text $ Normal c]
inlineToParagraph (InlineCodeBlock c) = CodeBlockItem
    [ParagraphItem $ Content [ParagraphItem $ Text $ Normal c]]

listToParagraph :: Inline -> Item
listToParagraph inln = ParagraphItem $ Content [inlineToParagraph inln]

blockToItem :: Block -> Item
blockToItem (MdParagraph [InlineCodeBlock c]) = inlineToParagraph
    (InlineCodeBlock c)
blockToItem (MdParagraph v) = ParagraphItem $ Content (map inlineToParagraph v)
blockToItem (MdSection n v) = SectionItem $
    Section (Just n) (map blockToItem v)
blockToItem (MdList v) = ListItem $ map listToParagraph v
