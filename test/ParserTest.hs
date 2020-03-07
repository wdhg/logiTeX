module ParserTest (tests) where

import Lexer
import Parser
import Test.HUnit

text, question :: Token
text
  = Token Text "some text"
question
  = Token Question "question"

pushTests :: Test
pushTests
  = TestList
    [ push (Leaf question) (Leaf text) ~?= Node question [Leaf text]
    , push (Node question []) (Leaf text) ~?= Node question [Leaf text]
    , push (Root []) (Leaf text) ~?= Root [Leaf text]
    ]

tests :: Test
tests
  = TestList
    [ "push" ~: pushTests
    ]
