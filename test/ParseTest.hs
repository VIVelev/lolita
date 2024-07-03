-- | Test the parser
-- Thanks Claude! :)
module ParseTest where

import Data.String (IsString (fromString))
import Parse
import Test.HUnit

-- Helper function to create a test case for successful parsing
testCase :: String -> Either Error SExp -> Parse.State -> Test
testCase name expected input =
  TestCase $ assertEqual name expected (runParser sexp input)

-- Helper function to check if parsing fails
assertParserFails :: String -> Parse.State -> Test
assertParserFails name input =
  TestCase $ assertBool name (isLeft (runParser sexp input))

-- Helper function to check if a value is Left (i.e. an Error)
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

{- ORMOLU_DISABLE -}
testCases :: Test
testCases = TestList [
  -- Successful cases
  testCase "Simple atom" (Right (Atom "abc")) (fromString "abc"),
  testCase "Simple pair" (Right (Pair (Atom "a") (Atom "b"))) (fromString "(a . b)"),
  testCase "Simple list" 
    (Right (Pair (Atom "a") (Pair (Atom "b") (Pair (Atom "c") Nil))))
    (fromString "(a b c)"),
  testCase "Nested list" 
    (Right (Pair (Atom "a") (Pair (Pair (Atom "b") (Pair (Atom "c") Nil)) Nil)))
    (fromString "(a (b c))"),
  testCase "Complex nested list"
    (Right (Pair (Atom "a") 
      (Pair (Pair (Atom "b") (Pair (Atom "c") Nil)) 
        (Pair (Atom "d") 
          (Pair (Pair (Atom "e") 
            (Pair (Pair (Atom "f") (Pair (Atom "g") Nil)) Nil)) Nil)))))
    (fromString "(a (b c) d (e (f g)))"),
  testCase "List with nil"
    (Right (Pair (Atom "a") (Pair (Atom "b") (Pair Nil Nil))))
    (fromString "(a b '())"),
  testCase "Whitespace handling"
    (Right (Pair (Atom "a") (Pair (Atom "b") Nil)))
    (fromString "  (  a  b  )  "),
  testCase "Empty nested list"
    (Right (Pair (Atom "a") (Pair Nil (Pair (Atom "b") Nil))))
    (fromString "(a '() b)"),
  testCase "Multiple nested lists"
    (Right (Pair 
      (Pair (Atom "a") (Pair (Atom "b") Nil)) 
      (Pair (Pair (Atom "c") (Pair (Atom "d") Nil)) Nil)))
    (fromString "((a b) (c d))"),
  testCase "List with multiple nils"
    (Right (Pair (Atom "a") (Pair Nil (Pair (Atom "b") (Pair Nil (Pair (Atom "c") Nil))))))
    (fromString "(a '() b '() c)"),

  -- Failure cases
  assertParserFails "Empty list" (fromString "()"),
  assertParserFails "Unmatched opening parenthesis" (fromString "(a b c"),
  assertParserFails "Unmatched closing parenthesis" (fromString "a b c)"),
  assertParserFails "Invalid character in atom" (fromString "ab@c"),
  assertParserFails "Empty input" (fromString ""),
  assertParserFails "Misplaced dot" (fromString "(a . b . c)"),
  assertParserFails "Incomplete pair" (fromString "(a .)")
  ]
{- ORMOLU_ENABLE -}

run :: IO Counts
run = runTestTT testCases
