import qualified CompilerTest (tests)
import qualified LexerTest    (tests)
import qualified ParserTest   (tests)
import           Test.HUnit
import qualified UtilsTest    (tests)

tests :: Test
tests
  = TestList
    [ CompilerTest.tests
    , LexerTest.tests
    , ParserTest.tests
    , UtilsTest.tests
    ]

main :: IO Counts
main
  = runTestTT tests
