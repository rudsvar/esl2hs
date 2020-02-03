import Test.Hspec

import LibSpec
import ParserSpec

main :: IO ()
main = hspec $ do
  describe "Lib"    LibSpec.spec
  describe "Parser" ParserSpec.spec
