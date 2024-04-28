{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-LYN-4-1-mypandoc-mael.rabot
-- File description:
-- Types.hs
-}

module ArgsParser (
    Args(..),
    parseArgs
) where

data Args = Args {
    inputFile :: Maybe String,
    inputFormat :: Maybe String,
    outputFile :: Maybe String,
    outputFormat :: Maybe String
} deriving (Show, Eq)

defaultArgs :: Args
defaultArgs = Args {
    inputFile = Nothing,
    inputFormat = Nothing,
    outputFile = Nothing,
    outputFormat = Nothing
}

parseArgs' :: [String] -> Args -> Args
parseArgs' [] args = args
parseArgs' ("-i":inputFileStr:rest) args =
  parseArgs' rest (args {inputFile = Just inputFileStr})
parseArgs' ("-f":outputFormatStr:rest) args =
  parseArgs' rest (args {outputFormat = Just outputFormatStr})
parseArgs' ("-o":outputFileStr:rest) args =
  parseArgs' rest (args {outputFile = Just outputFileStr})
parseArgs' ("-e":inputFormatStr:rest) args =
  parseArgs' rest (args {inputFormat = Just inputFormatStr})
parseArgs' _ _ = defaultArgs

checkArgs :: Args -> Either String Args
checkArgs args@(Args {inputFile = Just _}) = Right args
checkArgs _ = Left "inputFile must be present"

checkOutputFormat :: Args -> Either String Args
checkOutputFormat args@(Args {outputFormat = Just format})
    | format `elem` ["markdown", "json", "xml"] = Right args
    | otherwise = Left "outputFormat must be either markdown, json, or xml"
checkOutputFormat _ = Left "outputFormat must be present"

checkInputFormat :: Args -> Either String Args
checkInputFormat args@(Args {inputFormat = Just format})
    | format `elem` ["markdown", "json", "xml"] = Right args
    | otherwise = Left "inputFormat must be either markdown, json, or xml"
checkInputFormat args = Right args

parseArgs :: [String] -> Either String Args
parseArgs args = do
    let parsedArgs = parseArgs' args defaultArgs
    checkedArgs <- checkArgs parsedArgs
    checkedArgs <- checkOutputFormat checkedArgs
    checkInputFormat checkedArgs
