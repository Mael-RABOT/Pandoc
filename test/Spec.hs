import System.Exit (exitWith, ExitCode(..))
import Test.HUnit
import Json
import Xml (parseXml)
import Parse (Parser(..))

tests :: Test
tests = test [
    "test1" ~: assertEqual "Testing parseJson with null" (Right (JsonNull, ".")) (runParser parseJson "null"),
    "test2" ~: assertEqual "Testing parseJson with true" (Right ((JsonBool True), "")) (runParser parseJson "true"),
    "test3" ~: assertEqual "Testing parseJson with false" (Right ((JsonBool False), "")) (runParser parseJson "false"),
    "test4" ~: assertEqual "Testing parseJson with number" (Right ((JsonNumber 42), "")) (runParser parseJson "42"),
    "test5" ~: assertEqual "Testing parseJson with string" (Right ((JsonString "Hello"), "")) (runParser parseJson "\"Hello\""),
    "test6" ~: assertEqual "Testing parseJson with array" (Right ((JsonArray [JsonNumber 1, JsonNumber 2]), "")) (runParser parseJson "[1, 2]"),
    "test7" ~: assertEqual "Testing parseJson with object" (Right ((JsonObject [("key", JsonString "value")]), "")) (runParser parseJson "{\"key\": \"value\"}")
  ]

main :: IO ()
main = do
  counts <- runTestTT tests
  case counts of
    Counts { errors = 0, failures = 0 } -> pure ()
    _ -> exitWith (ExitFailure 1)
