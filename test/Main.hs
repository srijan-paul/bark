import Bark.FrontMatter (Token (..), tokenize)
import Test.HUnit (Assertion, Test (..), runTestTT, runTestTTAndExit, runTestText, (~=?))

tokenizerTests :: Test
tokenizerTests =
  TestList
    [ [TKey "foo", TKey "bar"] ~=? tokenize "foo bar",
      [TKey "foobar"] ~=? tokenize "foobar",
      [TLBrac, TRBrac] ~=? tokenize "[]",
      [TString "this is a string"] ~=? tokenize "\"this is a string\"",
      [TKey "foo", TColon, TString "bar"] ~=? tokenize "foo: \"bar\"",
      [TKey "foo", TColon, TString "bar", TKey "bar", TColon, TString "baz"] 
        ~=? tokenize "foo: \"bar\"\n bar: \"baz\"",
      [TLBrac, TString "aa", TString "bb", TRBrac ] ~=? tokenize "[\"aa\" \"bb\"]"
    ]

main :: IO ()
main = runTestTTAndExit tokenizerTests
