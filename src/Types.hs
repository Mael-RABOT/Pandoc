{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- Parse.hs
-}

module Types () where

type Optional = Maybe

data Header = Header {
    title :: String,
    auhtor :: Optional String,
    date :: Optional String
} deriving (Show, Eq)

data Text =
    Normal String
    | Italic String
    | Bold String
    | Code String
    deriving (Show, Eq)

data Item =
    Paragraph Text
    | List [Item]
    | Codeblock [Item]
    | Section Block
    deriving (Show, Eq)

data Block = Block {
    name :: Optional String,
    items :: [Item]
} deriving (Show, Eq)

data UniversalContent = UniversalContent {
    header :: Header,
    body :: [Item]
} deriving (Show, Eq)
