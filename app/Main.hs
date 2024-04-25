{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- Main.hs
-}

module Main ( main ) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Data.Maybe (fromJust)

import Json (parseJson, formatJson)
import Xml (parseXml, formatXml)
import Parse (Parser(..))
import Markdown ( formatMarkdown )
import ArgsParser (parseArgs, Args(..))
import JsonToUniversal ( jsonToUniversal )
import XmlToUniversalContent ( xmlToUniversalContent )
import Prelude
import Formatter ( Formatter, runFormatter )
import Types (UniversalContent(..))

getFormatter :: Args -> Formatter
getFormatter (Args _ _ _ (Just format)) = case format of
        "json"      -> formatJson
        "markdown"  -> formatMarkdown
        "xml"       -> formatXml
        _           -> formatMarkdown
getFormatter _ = formatMarkdown

jsonEngine :: String -> Either String UniversalContent
jsonEngine str = case runParser parseJson str of
    Right (json, _) -> jsonToUniversal $ Right json
    Left err ->  Left err

xmlEngine :: String -> Either String UniversalContent
xmlEngine str = case runParser parseXml str of
    Right (xml, _) -> xmlToUniversalContent $ Right xml
    Left err ->  Left err


run :: Args -> IO ()
run args = do
    content <- readFile (fromJust $ inputFile args)
    case jsonEngine content of
        Right v ->
            putStr (runFormatter (getFormatter args) v)
        Left _ -> case xmlEngine content of
            Right v -> putStr (runFormatter (getFormatter args) v)
            _ -> putStr "err"


main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Right a -> run a
        Left errMsg -> putStrLn errMsg >> exitWith (ExitFailure 84)
    -- readFile "tests/example.xml" >>= \ c -> case (runParser parseXml c) of
    -- Left err -> print err
    -- Right (v, _) -> print $ xmlToUniversalContent v
