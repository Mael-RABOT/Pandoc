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

import Json (parseJson)
import Xml (parseXml)
import Markdown (parseMarkdown)
import Parse (Parser(..))
import ArgsParser (parseArgs, Args(..))
import JsonToUniversal (jsonToUniversal)
import PrintUniversalContent (printUniversalContent)
import DebugJson (printJson)

run :: Args -> IO ()
run args = do
    content <- readFile (fromJust $ inputFile args)
    case runParser parseJson content of
        Right (json, _) ->
            case jsonToUniversal json of
                Right universalContent ->
                  printUniversalContent universalContent
                Left err -> putStrLn err
        Left err -> putStrLn err

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Right args -> run args
        Left errMsg -> putStrLn errMsg >> exitWith (ExitFailure 84)
