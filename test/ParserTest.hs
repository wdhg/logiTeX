module ParserTest (tests) where

import Lexer
import Parser
import Test.HUnit

pushTests :: Test
pushTests
  = TestList
    [ push (Leaf question) (Leaf text) ~?= Node question [Leaf text]
    , push (Node question []) (Leaf text) ~?= Node question [Leaf text]
    , push (Root []) (Leaf text) ~?= Root [Leaf text]
    ]
    where
      text
        = Token Text "some text"
      question
        = Token Question "question"

tests :: Test
tests
  = TestList
    [ "push" ~: pushTests
    ]
