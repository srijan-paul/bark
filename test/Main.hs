import Bark.FrontMatter (Token (..), tokenize, parse, parseFromTokens)
import Test.HUnit (Assertion, Test (..), runTestTT, runTestTTAndExit, runTestText, (~=?), assertEqual)
import Text.Mustache.Types ( Value(..) )
import Data.Text (pack)

tokenizerTests :: [Test]
tokenizerTests =
    [ [TKey "foo", TKey "bar"] ~=? tokenize "foo bar",
      [TKey "foobar"] ~=? tokenize "foobar",
      [TLSqBrac, TRSqBrac] ~=? tokenize "[]",
      [TString "this is a string"] ~=? tokenize "\"this is a string\"",
      [TKey "foo", TColon, TString "bar"] ~=? tokenize "foo: \"bar\"",
      [TKey "foo", TColon, TString "bar", TKey "bar", TColon, TString "baz"] 
        ~=? tokenize "foo: \"bar\"\n bar: \"baz\"",
      [TLSqBrac, TString "aa", TString "bb", TRSqBrac ] ~=? tokenize "[\"aa\" \"bb\"]",
      [TLBrac, TString "aa", TString "bb", TRBrac ] ~=? tokenize "{\"aa\" \"bb\"}"
    ]

-- Mustache Value data class does not derive `Eq`, so the next best way to test them
-- that I could come up with was to stringize them with `show` and compare with expected values.
-- Haxskel :)
parserTests :: [Test]
parserTests =
  [
    show (parseFromTokens [TString "hello world"]) ~=? "Right \"hello world\"",
    show (parseFromTokens [TLSqBrac, TString "a", TString "b", TRSqBrac ]) ~=? "Right [\"a\",\"b\"]",
    show (parseFromTokens [TLSqBrac, TRSqBrac]) ~=? "Right []"
  ]

main :: IO ()
main = runTestTTAndExit $ TestList $ tokenizerTests ++ parserTests
