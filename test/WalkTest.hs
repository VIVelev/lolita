-- | Test the various code walkers.
-- Credit goes to Claude!
module WalkTest where

import Objectify
import Parse
import Test.HUnit
import Walk

-- Helper function to create a test case for walkers
testWalker :: String -> Program () () -> Program IsFreeMutable FreeVars -> Test
testWalker name input expected =
  TestCase $ assertEqual name expected (free . markMutable . recordMutable $ input)

{- ORMOLU_DISABLE -}
tests :: Test
tests = TestList [
  testWalker "Simple immutable variable"
    (Function [Variable "x" False ()] [Reference (Variable "x" False ())] ())
    (Function [Variable "x" False (IsFreeMutable False False)] [Reference (Variable "x" False (IsFreeMutable False False))] (FreeVars [])),

  testWalker "Mutable local variable"
    (Function [Variable "x" False ()] [Assignment (Variable "x" False ()) (Const (Atom (IntLiteral 1)))] ())
    (Function [Variable "x" False (IsFreeMutable False True)] [Assignment (Variable "x" False (IsFreeMutable False True)) (Const (Atom (IntLiteral 1)))] (FreeVars [])),

  testWalker "Free variable"
    (Function [Variable "x" False ()] [Reference (Variable "y" False ())] ())
    (Function [Variable "x" False (IsFreeMutable False False)] [Reference (Variable "y" False (IsFreeMutable True False))] (FreeVars [Variable "y" False (IsFreeMutable True False)])),

  testWalker "Nested functions with free and mutable variables"
    (Function [Variable "x" False ()] 
      [Function [Variable "y" False ()] 
        [Assignment (Variable "x" False ()) (Const (Atom (IntLiteral 1))),
         Reference (Variable "z" False ())] ()] ())
    (Function [Variable "x" False (IsFreeMutable False True)] 
      [Function [Variable "y" False (IsFreeMutable False False)] 
        [Assignment (Variable "x" False (IsFreeMutable True True)) (Const (Atom (IntLiteral 1))),
         Reference (Variable "z" False (IsFreeMutable True False))] 
        (FreeVars [Variable "x" False (IsFreeMutable True True), Variable "z" False (IsFreeMutable True False)])] 
      (FreeVars [Variable "z" False (IsFreeMutable True False)])),

  testWalker "Complex nested functions with multiple variables"
    (Function [Variable "a" False (), Variable "b" False ()] 
      [Assignment (Variable "a" False ()) (Const (Atom (IntLiteral 1))),
       Function [Variable "c" False ()] 
         [Reference (Variable "b" False ()),
          Assignment (Variable "d" False ()) (Const (Atom (IntLiteral 2))),
          Reference (Variable "e" False ())] ()] ())
    (Function [Variable "a" False (IsFreeMutable False True), Variable "b" False (IsFreeMutable False False)] 
      [Assignment (Variable "a" False (IsFreeMutable False True)) (Const (Atom (IntLiteral 1))),
       Function [Variable "c" False (IsFreeMutable False False)] 
         [Reference (Variable "b" False (IsFreeMutable True False)),
          Assignment (Variable "d" False (IsFreeMutable True True)) (Const (Atom (IntLiteral 2))),
          Reference (Variable "e" False (IsFreeMutable True False))] 
         (FreeVars [Variable "b" False (IsFreeMutable True False), Variable "d" False (IsFreeMutable True True), Variable "e" False (IsFreeMutable True False)])] 
      (FreeVars [Variable "d" False (IsFreeMutable True True), Variable "e" False (IsFreeMutable True False)]))
  ]
{- ORMOLU_ENABLE -}

run :: IO Counts
run = runTestTT tests
