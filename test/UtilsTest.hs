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

tests :: Test
tests
  = TestList
    [ "takeFirstWord" ~: takeFirstWordTests
    , "trim" ~: trimTests
    ]
