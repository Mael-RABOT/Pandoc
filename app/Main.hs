{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- Main.hs
-}

module Main where

import Json (parseJson)
import Xml (parseXml)
import Parse (Parser(..))

main :: IO ()
-- main = pure ()
main = do
    readFile "test/example.json" >>= print . runParser parseJson
    readFile "test/example.xml" >>= print . runParser parseXml
