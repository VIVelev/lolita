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
tests :: Test
tests = TestList [
  -- Successful cases
  testCase "Simple atom (symbol)" (Right (Atom (Symbol "abc"))) (fromString "abc"),
  testCase "Simple atom (integer)" (Right (Atom (IntLiteral 123))) (fromString "123"),
  testCase "Simple atom (boolean true)" (Right (Atom (BoolLiteral True))) (fromString "#t"),
  testCase "Simple atom (boolean false)" (Right (Atom (BoolLiteral False))) (fromString "#f"),
  testCase "Simple pair" (Right (Pair (Atom (Symbol "a")) (Atom (Symbol "b")))) (fromString "(a . b)"),
  testCase "Simple list" 
    (Right (Pair (Atom (Symbol "a")) (Pair (Atom (Symbol "b")) (Pair (Atom (Symbol "c")) Nil))))
    (fromString "(a b c)"),
  testCase "Nested list" 
    (Right (Pair (Atom (Symbol "a")) (Pair (Pair (Atom (Symbol "b")) (Pair (Atom (Symbol "c")) Nil)) Nil)))
    (fromString "(a (b c))"),
  testCase "Complex nested list"
    (Right (Pair (Atom (Symbol "a")) 
      (Pair (Pair (Atom (Symbol "b")) (Pair (Atom (Symbol "c")) Nil)) 
        (Pair (Atom (Symbol "d")) 
          (Pair (Pair (Atom (Symbol "e")) 
            (Pair (Pair (Atom (Symbol "f")) (Pair (Atom (Symbol "g")) Nil)) Nil)) Nil)))))
    (fromString "(a (b c) d (e (f g)))"),
  testCase "List with nil"
    (Right (Pair (Atom (Symbol "a")) (Pair (Atom (Symbol "b")) (Pair Nil Nil))))
    (fromString "(a b '())"),
  testCase "Whitespace handling"
    (Right (Pair (Atom (Symbol "a")) (Pair (Atom (Symbol "b")) Nil)))
    (fromString "  (  a  b  )  "),
  testCase "Empty nested list"
    (Right (Pair (Atom (Symbol "a")) (Pair Nil (Pair (Atom (Symbol "b")) Nil))))
    (fromString "(a '() b)"),
  testCase "Multiple nested lists"
    (Right (Pair 
      (Pair (Atom (Symbol "a")) (Pair (Atom (Symbol "b")) Nil)) 
      (Pair (Pair (Atom (Symbol "c")) (Pair (Atom (Symbol "d")) Nil)) Nil)))
    (fromString "((a b) (c d))"),
  testCase "List with multiple nils"
    (Right (Pair (Atom (Symbol "a")) (Pair Nil (Pair (Atom (Symbol "b")) (Pair Nil (Pair (Atom (Symbol "c")) Nil))))))
    (fromString "(a '() b '() c)"),
  testCase "List with mixed types"
    (Right (Pair (Atom (Symbol "a")) (Pair (Atom (IntLiteral 123)) (Pair (Atom (BoolLiteral True)) Nil))))
    (fromString "(a 123 #t)"),
  testCase "Large integer" (Right (Atom (IntLiteral 1234567890))) (fromString "1234567890"),
  testCase "Negative integer" (Right (Atom (IntLiteral (-42)))) (fromString "-42"),
  testCase "Symbol with digits" (Right (Atom (Symbol "x123"))) (fromString "x123"),
  testCase "Symbol with hyphens" (Right (Atom (Symbol "hello-world"))) (fromString "hello-world"),
  testCase "Nested pair" 
    (Right (Pair (Atom (Symbol "a")) (Pair (Atom (Symbol "b")) (Pair (Atom (Symbol "c")) (Atom (Symbol "d"))))))
    (fromString "(a . (b . (c . d)))"),
  testCase "List with nested pairs"
    (Right (Pair (Atom (Symbol "a")) (Pair (Pair (Atom (Symbol "b")) (Atom (Symbol "c"))) (Pair (Atom (Symbol "d")) Nil))))
    (fromString "(a (b . c) d)"),
  testCase "Complex expression with all types"
    (Right (Pair (Atom (Symbol "define"))
             (Pair (Atom (Symbol "factorial"))
               (Pair
                 (Pair (Atom (Symbol "lambda"))
                   (Pair (Pair (Atom (Symbol "n")) Nil)
                     (Pair
                       (Pair (Atom (Symbol "if"))
                         (Pair
                           (Pair (Atom (Symbol "="))
                             (Pair (Atom (Symbol "n"))
                               (Pair (Atom (IntLiteral 0))
                            Nil)))
                            (Pair (Atom (IntLiteral 1))
                              (Pair
                                (Pair (Atom (Symbol "*"))
                                  (Pair (Atom (Symbol "n"))
                                    (Pair
                                      (Pair (Atom (Symbol "factorial"))
                                        (Pair
                                          (Pair (Atom (Symbol "-"))
                                            (Pair (Atom (Symbol "n"))
                                              (Pair (Atom (IntLiteral 1))
                                           Nil)))
                                       Nil))
                                 Nil)))
                        Nil))))
                  Nil)))
            Nil)))
    )
    (fromString "(define factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1))))))"),

  -- Failure cases
  assertParserFails "Unmatched opening parenthesis" (fromString "(a b c"),
  assertParserFails "Unmatched closing parenthesis" (fromString "a b c)"),
  assertParserFails "Invalid character in atom" (fromString "ab@c"),
  assertParserFails "Empty input" (fromString ""),
  assertParserFails "Misplaced dot" (fromString "(a . b . c)"),
  assertParserFails "Incomplete pair" (fromString "(a .)"),
  assertParserFails "Invalid boolean" (fromString "#true"),
  assertParserFails "Invalid integer" (fromString "12a34"),
  assertParserFails "Incomplete list" (fromString "(a b (c d)"),
  assertParserFails "Extra closing parenthesis" (fromString "(a b) c)")
  ]
{- ORMOLU_ENABLE -}

run :: IO Counts
run = runTestTT tests
