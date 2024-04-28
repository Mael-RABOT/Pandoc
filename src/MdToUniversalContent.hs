{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- mdToUniversalContent.hs
-}

module MdToUniversalContent (markdownToUniversalContent) where

import Markdown (Markdown(..), parseMarkdown, Block(..), TextData(..))
import Types (Header(..), Item(..), Paragraph(..), UniversalContent(..), Section(..), Links(..), Text(..))

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

inlineToParagraph :: Inline -> Paragraph
inlineToParagraph (TextData t) = textDataToText t
inlineToParagraph (TextData t) = textDataToText

blockToItem :: Block -> Item
blockToItem (MdParagraph v) = ParagraphItem inlineToParagraph
blockToItem (MdSection n v) = ListItem []
blockToItem (MdList v) = ListItem []