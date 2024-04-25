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
    let title = fromMaybe "" $ lookup "title" jsonObject >>= getString
        author = lookup "author" jsonObject >>= getString
        date = lookup "date" jsonObject >>= getString
    in Header title author date

getString :: JsonValue -> Maybe String
getString (JsonString s) = Just s
getString _ = Nothing

getArray :: JsonValue -> Maybe [JsonValue]
getArray (JsonArray arr) = Just arr
getArray _ = Nothing

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
              (Just title) (getSectionContent content)
        _ -> Nothing

getSectionContent :: [JsonValue] -> [Item]
getSectionContent arr = mapMaybe getBodyItem arr

toBold :: [JsonValue] -> Text
toBold value = Bold (getStringValue $ head value)

toItalic :: [JsonValue] -> Text
toItalic value = Italic (getStringValue $ head value)

toCode :: [JsonValue] -> Text
toCode value = Code (getStringValue $ head value)

getOtherItem :: JsonValue -> Maybe Item
getOtherItem (JsonArray jsonArray) = Just $
  ParagraphItem $ Content $ mapMaybe getBodyItem jsonArray
getOtherItem (JsonObject dict) =
    let (keys, value) = unzip dict
    in case head keys of
        "bold" -> Just $ ParagraphItem $ Text (toBold value)
        "italic" -> Just $ ParagraphItem $ Text (toItalic value)
        "code" -> Just $ ParagraphItem $ Text (toCode value)
        "codeblock" -> Just $ CodeBlockItem $ getStringValue $ head value
        "link" -> getLinkItem value
        "image" -> getImageItem value
        _ -> Nothing
getOtherItem (JsonString str) = Just $ ParagraphItem $ Text (Normal str)
getOtherItem _ = Nothing

getLinkItem :: [JsonValue] -> Maybe Item
getLinkItem [JsonObject link] =
    case (lookup "url" link, lookup "content" link) of
        (Just (JsonString url), Just (JsonArray [JsonString content])) ->
            Just $ LinksItem $ Link url content
        _ -> Nothing
getLinkItem _ = Nothing

getImageItem :: [JsonValue] -> Maybe Item
getImageItem [JsonObject image] =
    case (lookup "url" image, lookup "alt" image) of
        (Just (JsonString url), Just (JsonArray [JsonString alt])) ->
            Just $ LinksItem $ Image url alt
        _ -> Nothing
getImageItem _ = Nothing

getStringValue :: JsonValue -> String
getStringValue (JsonString s) = s
getStringValue _ = ""
