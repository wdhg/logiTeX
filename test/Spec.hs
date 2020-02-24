import qualified LexerTest  (tests)
import           Test.HUnit
import qualified UtilsTest  (tests)

tests :: Test
tests
  = TestList
    [ UtilsTest.tests
    , LexerTest.tests
    ]

main :: IO Counts
main
  = runTestTT tests
