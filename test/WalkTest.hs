-- | Test the various code walkers.
-- Credit goes to Claude!
module WalkTest where

import Objectify
import Parse
import Test.HUnit
import Walk

-- Helper function to create a test case for walkers
testWalker :: String -> Program () () -> Program IsFreeMutablePrimitive FreeVars -> Test
testWalker name input expected =
  TestCase $ assertEqual name expected (free . markMutable . recordMutable . primitive $ input)

{- ORMOLU_DISABLE -}
tests :: Test
tests = TestList [
  testWalker "Simple immutable variable"
    (Function [Variable "x" False ()] [Reference (Variable "x" False ())] ())
    (Function [Variable "x" False (IsFreeMutablePrimitive False False Nothing)] [Reference (Variable "x" False (IsFreeMutablePrimitive False False Nothing))] (FreeVars [])),

  testWalker "Mutable local variable"
    (Function [Variable "x" False ()] [Assignment (Variable "x" False ()) (Const (Atom (IntLiteral 1)))] ())
    (Function [Variable "x" False (IsFreeMutablePrimitive False True Nothing)] [Assignment (Variable "x" False (IsFreeMutablePrimitive False True Nothing)) (Const (Atom (IntLiteral 1)))] (FreeVars [])),

  testWalker "Free variable"
    (Function [Variable "x" False ()] [Reference (Variable "y" False ())] ())
    (Function [Variable "x" False (IsFreeMutablePrimitive False False Nothing)] [Reference (Variable "y" False (IsFreeMutablePrimitive True False Nothing))] (FreeVars [Variable "y" False (IsFreeMutablePrimitive True False Nothing)])),

  testWalker "Nested functions with free and mutable variables"
    (Function [Variable "x" False ()] 
      [Function [Variable "y" False ()] 
        [Assignment (Variable "x" False ()) (Const (Atom (IntLiteral 1))),
         Reference (Variable "z" False ())] ()] ())
    (Function [Variable "x" False (IsFreeMutablePrimitive False True Nothing)] 
      [Function [Variable "y" False (IsFreeMutablePrimitive False False Nothing)] 
        [Assignment (Variable "x" False (IsFreeMutablePrimitive True True Nothing)) (Const (Atom (IntLiteral 1))),
         Reference (Variable "z" False (IsFreeMutablePrimitive True False Nothing))] 
        (FreeVars [Variable "x" False (IsFreeMutablePrimitive True True Nothing), Variable "z" False (IsFreeMutablePrimitive True False Nothing)])] 
      (FreeVars [Variable "z" False (IsFreeMutablePrimitive True False Nothing)])),

  testWalker "Complex nested functions with multiple variables"
    (Function [Variable "a" False (), Variable "b" False ()] 
      [Assignment (Variable "a" False ()) (Const (Atom (IntLiteral 1))),
       Function [Variable "c" False ()] 
         [Reference (Variable "b" False ()),
          Assignment (Variable "d" False ()) (Const (Atom (IntLiteral 2))),
          Reference (Variable "e" False ())] ()] ())
    (Function [Variable "a" False (IsFreeMutablePrimitive False True Nothing), Variable "b" False (IsFreeMutablePrimitive False False Nothing)] 
      [Assignment (Variable "a" False (IsFreeMutablePrimitive False True Nothing)) (Const (Atom (IntLiteral 1))),
       Function [Variable "c" False (IsFreeMutablePrimitive False False Nothing)] 
         [Reference (Variable "b" False (IsFreeMutablePrimitive True False Nothing)),
          Assignment (Variable "d" False (IsFreeMutablePrimitive True True Nothing)) (Const (Atom (IntLiteral 2))),
          Reference (Variable "e" False (IsFreeMutablePrimitive True False Nothing))] 
         (FreeVars [Variable "b" False (IsFreeMutablePrimitive True False Nothing), Variable "d" False (IsFreeMutablePrimitive True True Nothing), Variable "e" False (IsFreeMutablePrimitive True False Nothing)])] 
      (FreeVars [Variable "d" False (IsFreeMutablePrimitive True True Nothing), Variable "e" False (IsFreeMutablePrimitive True False Nothing)]))
  ]
{- ORMOLU_ENABLE -}

run :: IO Counts
run = runTestTT tests
