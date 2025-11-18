module Tests.BufferTests (tests) where

import Buffer
import qualified Data.Sequence as S
import Test.Tasty
import Test.Tasty.HUnit

-- Helper to create an empty buffer
emptyBuffer :: Buffer
emptyBuffer = Buffer S.empty 0

-- Helper to create a buffer from a string, with cursor at the end
bufferFromString :: String -> Buffer
bufferFromString s = Buffer (S.fromList s) (length s)

tests :: TestTree
tests =
  testGroup
    "Buffer Tests"
    [ insertAtTests
    , deleteAtTests
    , getStringTests
    ]

insertAtTests :: TestTree
insertAtTests =
  testGroup
    "insertAt"
    [ testCase "insert at beginning" $
        let buf = insertAt 0 'a' (bufferFromString "bc")
         in assertEqual "should be 'abc'" "abc" (getString buf)
    , testCase "insert in middle" $
        let buf = insertAt 1 'b' (bufferFromString "ac")
         in assertEqual "should be 'abc'" "abc" (getString buf)
    , testCase "insert at end" $
        let buf = insertAt 2 'c' (bufferFromString "ab")
         in assertEqual "should be 'abc'" "abc" (getString buf)
    ]

deleteAtTests :: TestTree
deleteAtTests =
  testGroup
    "deleteAt"
    [ testCase "delete at beginning (index 0)" $
        let buf = deleteAt 0 (bufferFromString "abc")
         in assertEqual "should do nothing" "abc" (getString buf)
    , testCase "delete at index 1" $
        let buf = deleteAt 1 (bufferFromString "abc")
         in assertEqual "should be 'bc'" "bc" (getString buf)
    , testCase "delete at index 2" $
        let buf = deleteAt 2 (bufferFromString "abc")
         in assertEqual "should be 'ac'" "ac" (getString buf)
    , testCase "delete at end (index 3)" $
        let buf = deleteAt 3 (bufferFromString "abc")
         in assertEqual "should be 'ab'" "ab" (getString buf)
    ]

getStringTests :: TestTree
getStringTests =
  testGroup
    "getString"
    [ testCase "empty buffer" $
        assertEqual "should be empty string" "" (getString emptyBuffer)
    , testCase "buffer with content" $
        assertEqual "should be 'abc'" "abc" (getString (bufferFromString "abc"))
    ]
