module LexerTest (tests) where

import Lexer
import Test.HUnit

splitOnTypeTests :: Test
splitOnTypeTests
  = TestList
    [ splitOnType "abc\ndef" ~?= ("", "abc\ndef")
    , splitOnType "%abc\ndef" ~?= ("%", "abc\ndef")
    , splitOnType "% abc\ndef" ~?= ("%", "abc\ndef")
    , splitOnType "@ abc\ndef" ~?= ("@", "abc\ndef")
    ]

getNextTokenTests :: Test
getNextTokenTests
  = TestList
    [ getNextToken "% abc\n% def" ~?= (Token Equation "abc", "% def")
    , getNextToken "abc\n# def" ~?= (Token Text "abc", "# def")
    , getNextToken "@@@\nabc\ndef\n@@@\nghi" ~?= (Token LatexMulti "abc\ndef", "ghi")
    ]

tokenizeTests :: Test
tokenizeTests
  = TestList
    [ tokenize "~a\n%b\n```\nc\n```" ~?= [Token Title "a", Token Equation "b", Token Snippet "c"]

    ]

tests :: Test
tests
  = TestList
    [ "splitOnType" ~: splitOnTypeTests
    , "getNextToken" ~: getNextTokenTests
    , "tokenize" ~: tokenizeTests
    ]
