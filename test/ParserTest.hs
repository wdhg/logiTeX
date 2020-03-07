module ParserTest (tests) where

import Lexer
import Parser
import Test.HUnit

t1, t2, t3, t4, q1, q2, sq1, sq2 :: Token
t1 = Token Text "text1"
t2 = Token Text "text2"
t3 = Token Text "text3"
t4 = Token Text "text4"
q1 = Token Question "question1"
q2 = Token Question "question2"
sq1 = Token SubQuestion "subQuestion3"
sq2 = Token SubQuestion "subQuestion4"

pushTests :: Test
pushTests
  = TestList
    [ push (Leaf q1) (Leaf t1) ~?= Node q1 [Leaf t1]
    , push (Node q1 []) (Leaf t1) ~?= Node q1 [Leaf t1]
    , push (Root []) (Leaf t1) ~?= Root [Leaf t1]
    ]

parseTests :: Test
parseTests
  = TestList
    [ parse [t1, q1, t2]
      ~?= Root [Node q1 [Leaf t2], Leaf t1]
    , parse [t1, q1, sq1, t2]
      ~?= Root [Node q1 [Node sq1 [Leaf t2]], Leaf t1]
    , parse [t1, q1, t2, q2, t3]
      ~?= Root [Node q2 [Leaf t3], Node q1 [Leaf t2], Leaf t1]
    , parse [t1, q1, sq1, t2, sq2, t3, q2, t4]
      ~?= Root [Node q2 [Leaf t4], Node q1 [Node sq2 [Leaf t3], Node sq1 [Leaf t2]], Leaf t1]
    ]

tests :: Test
tests
  = TestList
    [ "push" ~: pushTests
    , "parseTests" ~: parseTests
    ]
