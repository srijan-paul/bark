import Bark.FrontMatter (toMustacheValue)
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Yaml as Yml
import Test.HUnit (Test (..), runTestTTAndExit)
import qualified Text.Mustache.Types as M

main :: IO ()
main =
  runTestTTAndExit $ TestList []
