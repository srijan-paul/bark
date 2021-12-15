import Bark.FrontMatter (Token (..), tokenize)
import Test.HUnit (Assertion, Test (..), runTestTT, runTestTTAndExit, (~=?), runTestText)

tokenizerTests :: Test
tokenizerTests =
  TestList
    [ [TKey "foo", TKey "bar"] ~=? tokenize "foo bar",
      [TKey "foobar"] ~=? tokenize "foobar",
      [TLBrac, TRBrac] ~=? tokenize "[]"
    ]

main :: IO ()
main = runTestTTAndExit tokenizerTests
