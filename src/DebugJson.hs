{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- PrintUniversalContent.hs
-}

module DebugJson where

import Text.Printf (printf)
import Json (JsonValue(..))

printJson :: JsonValue -> IO ()
printJson JsonNull = putStrLn "null"
printJson (JsonBool b) = putStrLn $ "Bool: " ++ show b
printJson (JsonNumber n) = putStrLn $ "Number: " ++ show n
printJson (JsonString s) = putStrLn $ "String: " ++ s
printJson (JsonArray arr) =
    putStrLn "Array:" >>
    mapM_ printJson arr
printJson (JsonObject pairs) =
    putStrLn "Object:" >>
    mapM_ printPair pairs

printPair :: (String, JsonValue) -> IO ()
printPair (key, value) =
    printf "Key: %s, Value: " key >>
    printJson value
