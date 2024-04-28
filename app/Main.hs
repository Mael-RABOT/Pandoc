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
import Types (UniversalContent(..), Section (content))

getFormatter :: String -> Formatter
getFormatter outFormat = case outFormat of
        "json"      -> formatJson
        "markdown"  -> formatMarkdown
        "xml"       -> formatXml

jsonEngine :: String -> Either String UniversalContent
jsonEngine str = case runParser parseJson str of
    Right (json, _) -> jsonToUniversal $ Right json
    Left err ->  Left err

xmlEngine :: String -> Either String UniversalContent
xmlEngine str = case runParser parseXml str of
    Right (xml, _) -> xmlToUniversalContent $ Right xml
    Left err ->  Left err

markdownEngine :: String -> Either String UniversalContent
markdownEngine str = Left "no MD converter"
    -- case runParser parseMarkdown str of
    -- Right (markdown, _) -> markdownToUniversalContent $ Right markdown
    -- Left err ->  Left err

printWhere :: Maybe String -> String -> IO ()
printWhere Nothing str = putStr str
printWhere(Just fn) str = writeFile fn str

getEngine :: String -> (String -> Either String UniversalContent)
getEngine format = case format of
    "json"      -> jsonEngine
    "xml"       -> xmlEngine
    "markdown"  -> markdownEngine

runEngine :: String -> Args -> Either String String
runEngine content (Args _ (Just format) _ (Just out)) =
    case getEngine format content of
        Right ok -> Right (runFormatter (getFormatter out) ok)
        Left err -> Left err
runEngine content (Args _ _ _ (Just out)) =
    case jsonEngine content of
        Right ok -> Right (runFormatter (getFormatter out) ok)
        Left _ -> case xmlEngine content of
            Right ok -> Right (runFormatter (getFormatter out) ok)
            Left err -> case markdownEngine content of
                Right ok -> Right (runFormatter (getFormatter out) ok)
                Left err -> Left "err: could not convert"

run :: Args -> IO ()
run args = do
    content <- readFile (fromJust $ inputFile args)
    case runEngine content args of
        Right result -> printWhere (outputFile args) result
        Left errMsg -> putStrLn errMsg >> exitWith (ExitFailure 84)


main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Right a -> run a
        Left errMsg -> putStrLn errMsg >> exitWith (ExitFailure 84)
