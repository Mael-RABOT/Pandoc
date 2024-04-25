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
import Xml (parseXml)
import Parse (Parser(..))
import Markdown ( formatMarkdown )
import ArgsParser (parseArgs, Args(..))
import JsonToUniversal ( jsonToUniversal )
import PrintUniversalContent ( printUniversalContent )
import DebugJson ( printJson )
import Prelude
import Formatter ( Formatter, runFormatter )

getFormatter :: Args -> Formatter
getFormatter (Args _ _ _ (Just format)) = case format of
        "json"      -> formatJson
        "markdown"  -> formatMarkdown
        _           -> formatMarkdown
getFormatter _ = formatMarkdown

run :: Args -> IO ()
run args = do
    content <- readFile (fromJust $ inputFile args)
    case runParser parseJson content of
        Right (json, _) ->
            case jsonToUniversal json of
                Right universalContent ->
                  putStr (runFormatter (getFormatter args)
                    universalContent)
                Left err -> putStrLn err
        Left err -> putStrLn err

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Right args -> run args
        Left errMsg -> putStrLn errMsg >> exitWith (ExitFailure 84)
