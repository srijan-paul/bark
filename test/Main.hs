import Bark.FrontMatter (Token (..), tokenize, parse')
import Test.HUnit (Assertion, Test (..), runTestTT, runTestTTAndExit, runTestText, (~=?))
import Text.Mustache.Types ( Value(..) )
import Data.Text (pack)

tokenizerTests :: Test
tokenizerTests =
  TestList
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

main :: IO ()
main = runTestTTAndExit tokenizerTests
