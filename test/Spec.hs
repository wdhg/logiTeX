import qualified LexerTest  (tests)
import qualified ParserTest (tests)
import           Test.HUnit
import qualified UtilsTest  (tests)

tests :: Test
tests
  = TestList
    [ UtilsTest.tests
    , LexerTest.tests
    , ParserTest.tests
    ]

main :: IO Counts
main
  = runTestTT tests
