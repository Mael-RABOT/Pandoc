{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- JsonToUniversal.hs
-}

module JsonToUniversal where

import Data.Maybe (fromMaybe, mapMaybe)

import Json (parseJson, JsonValue(..))
import Types (Optional, Header(..), Item(..), Paragraph(..), UniversalContent(..), Section(..), Links(..), Text(..))

jsonToUniversal :: Either String JsonValue -> Either String UniversalContent
jsonToUniversal (Left err) = Left err
jsonToUniversal (Right (JsonObject jsonObject)) =
    case lookup "header" jsonObject of
        Just (JsonObject header) ->
            case lookup "body" jsonObject of
                Just body -> Right $ UniversalContent
                  (getHeader header) (fromMaybe [] $ getBody body)
                Nothing -> Left "No body found"
        Nothing -> Left "No header found"
jsonToUniversal _m = Left "Invalid JSON"

getHeader :: [(String, JsonValue)] -> Header
getHeader jsonObject =
    let titleStr = fromMaybe "" $ lookup "title" jsonObject >>= getString
        authorStr = lookup "author" jsonObject >>= getString
        dateStr = lookup "date" jsonObject >>= getString
    in Header titleStr authorStr dateStr

getString :: JsonValue -> Maybe String
getString (JsonString s) = Just s
getString _ = Nothing

getBody :: JsonValue -> Maybe [Item]
getBody (JsonArray jsonArray) = Just $ mapMaybe getBodyItem jsonArray

getBodyItem :: JsonValue -> Maybe Item
getBodyItem (JsonObject dict) =
    case lookup "section" dict of
        Just section -> getSectionItem section
        Nothing -> getOtherItem (JsonObject dict)
getBodyItem item = getOtherItem item

getSectionItem :: JsonValue -> Maybe Item
getSectionItem (JsonObject dict) =
    case (lookup "title" dict, lookup "content" dict) of
        (Just (JsonString title), Just (JsonArray content)) ->
            Just $ SectionItem $ Section
              (Just title) (getArrayContent content)
        _ -> Nothing

getArrayContent :: [JsonValue] -> [Item]
getArrayContent = mapMaybe getBodyItem

toBold :: [JsonValue] -> Text
toBold value = Bold (getStringValue $ head value)

toItalic :: [JsonValue] -> Text
toItalic value = Italic (getStringValue $ head value)

toCode :: [JsonValue] -> Text
toCode value = Code (getStringValue $ head value)

processJsonArray :: [JsonValue] -> Maybe Item
processJsonArray jsonArray = Just $ ParagraphItem $
  Content $ mapMaybe getBodyItem jsonArray

getOtherItem :: JsonValue -> Maybe Item
getOtherItem (JsonArray jsonArray) = processJsonArray jsonArray
getOtherItem (JsonObject dict) = let (keys, value) = unzip dict
    in case head keys of
        "bold"      -> Just $ ParagraphItem $ Text (toBold value)
        "italic"    -> Just $ ParagraphItem $ Text (toItalic value)
        "code"      -> Just $ ParagraphItem $ Text (toCode value)
        "link"      -> getLinkItem value
        "image"     -> getImageItem value
        "list"      -> getListItem value
        "codeblock" -> getCodeBlockItem value
        _           -> Nothing
getOtherItem (JsonString str) = Just $ ParagraphItem $ Text (Normal str)
getOtherItem _ = Nothing


getListItem :: [JsonValue] -> Maybe Item
getListItem jsonArray =
    Just $ ListItem (getArrayContent jsonArray)

getCodeBlockItem :: [JsonValue] -> Maybe Item
getCodeBlockItem jsonArray =
    Just $ CodeBlockItem (getArrayContent jsonArray)

getLinkItem :: [JsonValue] -> Maybe Item
getLinkItem [JsonObject link] =
    case (lookup "url" link, lookup "content" link) of
        (Just (JsonString url), Just (JsonArray cont)) ->
            Just $ LinksItem $ Link url (getArrayContent cont)
        _ -> Nothing
getLinkItem _ = Nothing

getImageItem :: [JsonValue] -> Maybe Item
getImageItem [JsonObject image] =
    case (lookup "url" image, lookup "alt" image) of
        (Just (JsonString url), Just (JsonArray alt)) ->
            Just $ LinksItem $ Image url (getArrayContent alt)
        _ -> Nothing
getImageItem _ = Nothing

getStringValue :: JsonValue -> String
getStringValue (JsonString s) = s
getStringValue _ = ""
