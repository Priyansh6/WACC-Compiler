import Test.Hspec

import qualified ParserSpec

main :: IO ()
main = hspec $ do
  describe "Parser.hs" ParserSpec.spec