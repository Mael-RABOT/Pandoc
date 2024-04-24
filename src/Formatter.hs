{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- Formatter.hs
-}

module Formatter ( Formatter(..) ) where

import Types ( UniversalContent, Header, Item )

data Formatter = Formatter {
    onHeader :: String -> Header -> String,
    onBody :: String -> [Item] -> String,
    begin :: String,
    end :: String -> String
}

