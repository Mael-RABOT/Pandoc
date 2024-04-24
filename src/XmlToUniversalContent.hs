{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- XmlToUniversalContent.hs
-}

module XmlToUniversalContent (xmlToUniversalContent) where

import Xml (TagValue(..), XmlTag(..))
import Types (Header(..), Item(..), Paragraph(..), UniversalContent(..), Section(..), Links(..), Text(..))
import Data.Either (partitionEithers)

findByKey :: [(String, String)] -> String -> Maybe String
findByKey ((k, v):xs) tc | tc == k = Just v
                            | otherwise = findByKey xs tc
findByKey [] _ = Nothing

findByName :: [TagValue] -> String -> Maybe XmlTag
findByName [] _ = Nothing
findByName (x:xs) n =
    case x of
        Tag tag ->
            if dataTitle tag == n
                then Just tag
                else findByName xs n
        _ -> findByName xs n

getTagText :: Maybe XmlTag -> Maybe String
getTagText Nothing = Nothing
getTagText (Just (XmlTag _ ((XmlText c):_) _)) = Just c
getTagText _ = Nothing

getHeader :: Maybe XmlTag -> Either String Header
getHeader Nothing = Left "Header not found"
getHeader (Just tag) = case findByKey (attributes tag) "title" of
    Nothing -> Left "Header's title field not found"
    Just t -> Right $ Header t
        (getTagText (findByName (dataContent tag) "author"))
        (getTagText (findByName (dataContent tag) "date"))

parapraphToItem :: [TagValue] -> Paragraph
parapraphToItem [(XmlText txt)] = Content $ [ParagraphItem $ Text $ Normal txt]
parapraphToItem v = Content $ map f2 v

f2 :: TagValue -> Item
f2 (XmlText v) = ParagraphItem $ Text $ Normal v
f2 (Tag v) = case tagToItem v of
    (Right i) -> i

getLink :: [TagValue] -> Maybe String -> Either String Item
getLink _ Nothing = Left "link's url attribute not found"
getLink c (Just url) = Right $ LinksItem $ Link url
    (extractString $ head c)

getImage :: [TagValue] -> Maybe String -> Either String Item
getImage _ Nothing = Left "image url attribute not found"
getImage c (Just url) = Right $ LinksItem $ Image url
    (extractString $ head c)

tagToItem :: XmlTag -> Either String Item
tagToItem (XmlTag "paragraph" c _) = Right $ ParagraphItem $
    parapraphToItem c
tagToItem (XmlTag "section" c attrs) = Right $ SectionItem $
    Section (findByKey attrs "title") (map f2 c)
tagToItem (XmlTag "list" c _) = Right $ ListItem $ map f2 c
tagToItem (XmlTag "codeblock" c _) = Right $ CodeBlockItem $
    extractString $ head c
tagToItem (XmlTag "bold" c _) = Right $ ParagraphItem $ Text $
    Bold $ extractString $ head c
tagToItem t = tagToItem2 t

tagToItem2 :: XmlTag -> Either String Item
tagToItem2 (XmlTag "italic" c _) = Right $ ParagraphItem $ Text $
    Italic $ extractString $ head c
tagToItem2 (XmlTag "code" c _) = Right $ ParagraphItem $
    Text $ Code $ extractString $ head c
tagToItem2 (XmlTag "link" c attrs) = getLink c $
    findByKey attrs "url"
tagToItem2 (XmlTag "image" c attrs) = getImage c $
    findByKey attrs "url"
tagToItem2 (XmlTag n _ _)  = Left $ "Unknow tag with name: " ++ n

extractString :: TagValue -> String
extractString (XmlText txt) = txt
extractString _ = ""

tagValueToItem :: TagValue -> Either String Item
tagValueToItem (XmlText txt) = Right $ ParagraphItem $ Text $ Normal txt
tagValueToItem (Tag tag) = case tagToItem tag of
    Left err -> Left err
    (Right item) -> Right item

getBody :: Maybe XmlTag -> Either String [Item]
getBody Nothing = Left "Body not found"
getBody (Just tag) = processList $ map tagValueToItem $ dataContent tag

convertDocument :: [TagValue] -> Either String UniversalContent
convertDocument v = case getHeader $ findByName v "header" of
    Left err -> Left $ "Document error: " ++ err
    Right h -> case getBody $ findByName v "body" of
        Left err -> Left $ "Document error: " ++ err
        Right b -> Right $ UniversalContent h b

xmlToUniversalContent :: TagValue -> Either String UniversalContent
xmlToUniversalContent (Tag tag) = case dataTitle tag of
    "document" -> convertDocument $ dataContent tag
    _ -> Left "Xml docmument must begin with <document> tag"
xmlToUniversalContent _ = Left ""

processList :: [Either String a] -> Either String [a]
processList eithers =
    let (lefts, rights) = partitionEithers eithers
    in if null lefts
       then Right rights
       else Left (head lefts)
