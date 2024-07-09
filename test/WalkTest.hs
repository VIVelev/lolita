-- | Test the various code walkers.
-- Credit goes to Claude!
module WalkTest where

import Objectify (LocalVariable (..), Program (..))
import Test.HUnit
import Walk (insertBox)

{- ORMOLU_DISABLE -}
tests :: Test
tests = TestList
  [ TestLabel "Test immutable local reference" testImmutableLocalReference
  , TestLabel "Test mutable local reference" testMutableLocalReference
  , TestLabel "Test immutable local assignment" testImmutableLocalAssignment
  , TestLabel "Test mutable local assignment" testMutableLocalAssignment
  , TestLabel "Test function with mutable variables" testFunctionWithMutableVars
  , TestLabel "Test nested function" testNestedFunction
  ]

testImmutableLocalReference :: Test
testImmutableLocalReference = TestCase $ assertEqual
  "Immutable local reference should not be boxed"
  (LocalReference (LocalVariable "x" False False))
  (insertBox (LocalReference (LocalVariable "x" False False)))

testMutableLocalReference :: Test
testMutableLocalReference = TestCase $ assertEqual
  "Mutable local reference should be boxed"
  (BoxRead (LocalVariable "x" True False))
  (insertBox (LocalReference (LocalVariable "x" True False)))

testImmutableLocalAssignment :: Test
testImmutableLocalAssignment = TestCase $ assertEqual
  "Immutable local assignment should not be boxed"
  (LocalAssignment (LocalVariable "x" False False) (IntLiteral 5))
  (insertBox (LocalAssignment (LocalVariable "x" False False) (IntLiteral 5)))

testMutableLocalAssignment :: Test
testMutableLocalAssignment = TestCase $ assertEqual
  "Mutable local assignment should be boxed"
  (BoxWrite (LocalVariable "x" True False) (IntLiteral 5))
  (insertBox (LocalAssignment (LocalVariable "x" True False) (IntLiteral 5)))

testFunctionWithMutableVars :: Test
testFunctionWithMutableVars = TestCase $ assertEqual
  "Function with mutable variables should be boxed"
  (Function
    [LocalVariable "x" True False, LocalVariable "y" False False]
    [ BoxCreate (LocalVariable "x" True False)
    , BoxWrite (LocalVariable "x" True False) (IntLiteral 5)
    , BoxRead (LocalVariable "x" True False)
    ])
  (insertBox (Function
    [LocalVariable "x" True False, LocalVariable "y" False False]
    [ LocalAssignment (LocalVariable "x" True False) (IntLiteral 5)
    , LocalReference (LocalVariable "x" True False)
    ]))

testNestedFunction :: Test
testNestedFunction = TestCase $ assertEqual
  "Nested function should be correctly boxed"
  (Function
    [LocalVariable "x" True False]
    [ BoxCreate (LocalVariable "x" True False)
    , Function
        [LocalVariable "y" True False]
        [ BoxCreate (LocalVariable "y" True False)
        , BoxWrite (LocalVariable "y" True False) (BoxRead (LocalVariable "x" True False))
        ]
    ])
  (insertBox (Function
    [LocalVariable "x" True False]
    [ Function
        [LocalVariable "y" True False]
        [ LocalAssignment (LocalVariable "y" True False) (LocalReference (LocalVariable "x" True False))
        ]
    ]))
{- ORMOLU_ENABLE -}

run :: IO Counts
run = runTestTT tests
