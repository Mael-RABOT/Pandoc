{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- XmlToUniversalContent.hs
-}

module XmlToUniversalContent (xmlToUniversalContent) where
import Data.Maybe (catMaybes)

import Xml (TagValue(..), XmlTag(..))
import Types (Optional, Header(..), Item(..), Paragraph(..), UniversalContent(..), Section(..), Links(..), Text(..))
import Data.Either (partitionEithers)

findByKey :: [(String, String)] -> String -> Maybe String
findByKey ((k, v):xs) tc | tc == k = Just v
                            | otherwise = findByKey xs tc
findByKey [] _ = Nothing

findByName :: [TagValue] -> String -> Maybe XmlTag
findByName [] _ = Nothing
findByName (x:xs) name =
    case x of
        Tag tag ->
            if dataTitle tag == name
                then Just tag
                else findByName xs name
        _ -> findByName xs name

getTagText :: Maybe XmlTag -> Maybe String
getTagText Nothing = Nothing
getTagText (Just (XmlTag _ ((XmlText c):_) _)) = Just c
getTagText _ = Nothing

getHeader :: Maybe XmlTag -> Either String Header
getHeader Nothing = Left "Header not found"
getHeader (Just tag) = case findByKey (attributes tag) "title" of
    Nothing -> Left "Header's title field not found"
    Just title -> Right $ Header title
        (getTagText (findByName (dataContent tag) "author"))
        (getTagText (findByName (dataContent tag) "date"))


parapraphToItem :: [TagValue] -> Paragraph
parapraphToItem [(XmlText txt)] = Content $ [ParagraphItem $ Text $ Normal txt]
parapraphToItem v = Content $ catMaybes $ map f2 v

f2 :: TagValue -> Maybe Item
f2 (XmlText v) = Just $ ParagraphItem $ Text $ Normal v
f2 (Tag v) = tagToItem v

tagToItem :: XmlTag -> Maybe Item
tagToItem (XmlTag "paragraph" content attrs) = Just $ ParagraphItem $ parapraphToItem content
tagToItem (XmlTag "section" content attrs) = Just $ SectionItem $ Section (findByKey attrs "title") (catMaybes $ map f2 content)
tagToItem (XmlTag "list" content attrs) = Just $ ListItem $ catMaybes $ map f2 content
tagToItem (XmlTag "codeblock" content attrs) = Just $ CodeBlockItem $ extractString $ head content
tagToItem (XmlTag "bold" content attrs) = Just $ ParagraphItem $ Text $ Bold $ extractString $ head content
tagToItem (XmlTag "italic" content attrs) = Just $ ParagraphItem $ Text $ Italic $ extractString $ head content
tagToItem (XmlTag "code" content attrs) = Just $ ParagraphItem $ Text $ Code $ extractString $ head content
tagToItem _ = Nothing

extractString :: TagValue -> String
extractString (XmlText txt) = txt
extractString _ = ""

tagValueToItem :: TagValue -> Either String Item
tagValueToItem (XmlText txt) = Right $ ParagraphItem $ Text $ Normal txt
tagValueToItem (Tag tag) = case tagToItem tag of
    Nothing -> Left $ "Unknow tag with name: " ++ (dataTitle tag)
    (Just item) -> Right item

getBody :: Maybe XmlTag -> Either String [Item]
getBody Nothing = Left "Body not found"
getBody (Just tag) = processList $ map tagValueToItem $ dataContent tag

convertDocument :: [TagValue] -> Either String UniversalContent
convertDocument v = case getHeader $ findByName v "header" of
    Left err -> Left $ "Document error: " ++ err
    Right header -> case getBody $ findByName v "body" of
        Left err -> Left $ "Document error: " ++ err
        Right body -> Right $ UniversalContent header body

xmlToUniversalContent :: TagValue -> Either String UniversalContent
xmlToUniversalContent (Tag tag) = case dataTitle tag of
    "document" -> convertDocument $ dataContent tag
    _ -> Left "Xml docmument must begin with <document> tag"

processList :: [Either String a] -> Either String [a]
processList eithers =
    let (lefts, rights) = partitionEithers eithers
    in if null lefts
       then Right rights
       else Left (head lefts)
