module UtilsTest (tests) where

import Test.HUnit
import Utils

takeFirstWordTests :: Test
takeFirstWordTests
  = TestList
    [ takeFirstWord "a b c" ~?= "a"
    , takeFirstWord "aaa" ~?= "aaa"
    , takeFirstWord "" ~?= ""
    , takeFirstWord "aaa b c" ~?= "aaa"
    ]

trimTests :: Test
trimTests
  = TestList
    [ trim "  aaa" ~?= "aaa"
    , trim "aaa  " ~?= "aaa"
    , trim "  aaa  " ~?= "aaa"
    , trim "aaa" ~?= "aaa"
    ]

startsWithTests :: Test
startsWithTests
  = TestList
    [ startsWith "a" "ab" ~? "incorrect"
    , (not $ startsWith "b" "ab") ~? "incorrect"
    , startsWith "ab" "ab" ~? "incorrect"
    ]

splitOnTests :: Test
splitOnTests
  = TestList
    [ splitOn "b" "aba" ~?= ("a", "a")
    , splitOn "a" "aba" ~?= ("", "ba")
    , splitOn "c" "aba" ~?= ("aba", "")
    ]

tests :: Test
tests
  = TestList
    [ "takeFirstWord" ~: takeFirstWordTests
    , "trim" ~: trimTests
    , "startsWith" ~: startsWithTests
    , "splitOn" ~: splitOnTests
    ]
