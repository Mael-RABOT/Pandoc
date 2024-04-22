module Main where

import Json (parseJson)
import Xml (parseXml)
import Parse (Parser(..))

main :: IO ()
-- main = pure ()
main = do
    readFile "test/example.json" >>= print . runParser parseJson
    readFile "test/example.xml" >>= print . runParser parseXml
