{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- Types.hs
-}

module Types ( UniversalContent(..)
    , Header(..)
    , Links(..)
    , Text(..)
    , Paragraph(..)
    , Section(..)
    , Item(..)
    , Optional(..)
    ) where

type Optional = Maybe

data Header = Header {
    title :: String,
    author :: Optional String,
    date :: Optional String
} deriving (Show, Eq)

data Links =
  Link String String
  | Image String String
  deriving (Show, Eq)

data Text =
    Normal String
    | Italic String
    | Bold String
    | Code String
    deriving (Eq)

instance Show Text where
    show (Normal str) = "normal: " ++ str
    show (Italic str) = "italic: " ++ str
    show (Bold str) = "bold: " ++ str
    show (Code str) = "code: " ++ str

data Paragraph =
    Text Text
    | Content [Item]
    deriving (Show, Eq)

data Section = Section {
    name :: Optional String,
    content :: [Item]
} deriving (Show, Eq)

data Item =
    ParagraphItem Paragraph
    | ListItem [Item]
    | SectionItem Section
    | CodeBlockItem [Item]
    | LinksItem Links
    deriving (Show, Eq)

data UniversalContent = UniversalContent {
    header :: Header,
    body :: [Item]
} deriving (Show, Eq)
