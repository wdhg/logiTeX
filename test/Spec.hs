import           Test.HUnit
import qualified UtilsTest  (tests)

tests :: Test
tests
  = TestList
    [ UtilsTest.tests
    ]

main :: IO Counts
main
  = runTestTT tests
