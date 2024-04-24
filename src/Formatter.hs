{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- Formatter.hs
-}

module Formatter ( Formatter(..) ) where

import Types ( UniversalContent(..), Header, Item )

data Formatter = Formatter {
    begin :: String,
    onHeader :: String -> Header -> String,
    onBody :: String -> [Item] -> String,
    end :: String -> String
}

runFormatter :: Formatter -> UniversalContent -> String
runFormatter (Formatter begin onHeader onBody end)
    (UniversalContent header body) =
        let headerResult = onHeader begin header
            bodyResult = onBody headerResult body
            finalResult = end bodyResult
        in finalResult
