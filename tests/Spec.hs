import System.Exit (exitWith, ExitCode(..))
import Test.HUnit
import Json
import Xml
import Parse (Parser(..))

tests :: Test
tests = test [
    "Json - test1" ~: assertEqual "Testing parseJson with null" (Right (JsonNull, "")) (runParser parseJson "null"),
    "Json - test2" ~: assertEqual "Testing parseJson with true" (Right ((JsonBool True), "")) (runParser parseJson "true"),
    "Json - test3" ~: assertEqual "Testing parseJson with false" (Right ((JsonBool False), "")) (runParser parseJson "false"),
    "Json - test4" ~: assertEqual "Testing parseJson with number" (Right ((JsonNumber 42), "")) (runParser parseJson "42"),
    "Json - test5" ~: assertEqual "Testing parseJson with string" (Right ((JsonString "Hello"), "")) (runParser parseJson "\"Hello\""),
    "Json - test6" ~: assertEqual "Testing parseJson with array" (Right ((JsonArray [JsonNumber 1, JsonNumber 2]), "")) (runParser parseJson "[1, 2]"),
    "Json - test7" ~: assertEqual "Testing parseJson with object" (Right ((JsonObject [("key", JsonString "value")]), "")) (runParser parseJson "{\"key\": \"value\"}"),
    "Xml - test1" ~: assertEqual "Testing parseXml with simple tag" (Right ((Tag (XmlTag "tag" [] [])), "")) (runParser parseXml "<tag></tag>"),
    "Xml - test2" ~: assertEqual "Testing parseXml with tag having text" (Right ((Tag (XmlTag "tag" [XmlText "Hello"] [])), "")) (runParser parseXml "<tag>Hello</tag>"),
    "Xml - test3" ~: assertEqual "Testing parseXml with tag having attribute" (Right ((Tag (XmlTag "tag" [] [("attr", "value")])), "")) (runParser parseXml "<tag attr=\"value\"></tag>"),
    "Xml - test4" ~: assertEqual "Testing parseXml with tag having multiple attributes" (Right ((Tag (XmlTag "tag" [] [("attr1", "value1"), ("attr2", "value2")])), "")) (runParser parseXml "<tag attr1=\"value1\" attr2=\"value2\"></tag>"),
    "Xml - test5" ~: assertEqual "Testing parseXml with nested tags" (Right ((Tag (XmlTag "outer" [Tag (XmlTag "inner" [] [])] [])), "")) (runParser parseXml "<outer><inner></inner></outer>"),
    "Xml - test6" ~: assertEqual "Testing parseXml with nested tags having text" (Right ((Tag (XmlTag "outer" [Tag (XmlTag "inner" [XmlText "Hello"] [])] [])), "")) (runParser parseXml "<outer><inner>Hello</inner></outer>")
  ]

main :: IO ()
main = do
  counts <- runTestTT tests
  case counts of
    Counts { errors = 0, failures = 0 } -> pure ()
    _ -> exitWith (ExitFailure 1)
