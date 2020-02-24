module LexerTest (tests) where

import Lexer
import Test.HUnit

getNextTokenTests :: Test
getNextTokenTests
  = TestList
    [ getNextToken "% abc\n% def" ~?= (Token Equation "abc", "% def")
    , getNextToken "abc\n# def" ~?= (Token Text "abc", "# def")
    , getNextToken "@@@\nabc\ndef\n@@@\nghi" ~?= (Token LatexMulti "abc\ndef", "ghi")
    ]

tests :: Test
tests
  = TestList
    [ "getNextToken" ~: getNextTokenTests
    ]
