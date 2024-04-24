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
import Parse (Parser(..))
import ArgsParser (Args(..), parseArgs)

run :: Args -> IO ()
run args = do
    content <- readFile (fromJust $ inputFile args)
    case inputFormat args of
        Just "json" -> print . runParser parseJson $ content
        Just "xml" -> print . runParser parseXml $ content
        _ -> putStrLn "Invalid input format"

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Right args -> run args
        Left errMsg -> putStrLn errMsg >> exitWith (ExitFailure 84)