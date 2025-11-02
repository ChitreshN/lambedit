module Tests.DocTests (tests) where

import Buffer
import qualified Data.Sequence as S
import Doc
import Keys
import Test.Tasty
import Test.Tasty.HUnit

-- Helper to create a buffer from a string
bufferFromString :: String -> Buffer
bufferFromString s = Buffer (S.fromList s) S.empty (length s)

tests :: TestTree
tests =
  testGroup
    "Doc Tests"
    [ initDocTests
    , updateDocTests
    ]

initDocTests :: TestTree
initDocTests =
  testGroup
    "initDoc"
    [ testCase "Initial document has one empty line" $
        let doc = initDoc
         in do
              assertEqual "content should have zero buffers" 0 (S.length (content doc))
              assertEqual "cursor should be at (0,0)" (0, 0) (cursor doc)
              assertEqual "depth should be 0" 0 (depth doc)
    ]

updateDocTests :: TestTree
updateDocTests =
  testGroup
    "updateDoc"
    [ testCase "write text to empty doc" $
        let doc = foldl updateDoc initDoc [Key 'a', Key 'b', Key 'c']
            expectedContent = S.singleton (bufferFromString "abc")
         in do
              assertEqual
                "content should be 'abc'"
                (fmap getString expectedContent)
                (fmap getString (content doc))
              assertEqual "cursor should be at (0,3)" (0, 3) (cursor doc)
              assertEqual "depth should be 0" 0 (depth doc)
    , testCase "arrow keys movement" $
        let doc = foldl updateDoc initDoc [Key 'a', Key 'b', Key 'c', ArrowLeft, ArrowUp]
         in do
              assertEqual "cursor should be at (0,2)" (0, 2) (cursor doc)
    , testCase "insert in the middle" $
        let doc = foldl updateDoc initDoc [Key 'a', Key 'b', Key 'c', ArrowLeft, Key 'd']
            expectedContent = S.singleton (bufferFromString "abdc")
         in do
              assertEqual
                "content should be 'abdc'"
                (fmap getString expectedContent)
                (fmap getString (content doc))
              assertEqual "cursor should be at (0,3)" (0, 3) (cursor doc)
    , testCase "new line creates a new line" $
        let doc = foldl updateDoc initDoc [Key 'a', Key 'b', NewLine, Key 'c']
            expectedContent = S.fromList [bufferFromString "ab", bufferFromString "c"]
         in do
              assertEqual
                "content should have two lines"
                (fmap getString expectedContent)
                (fmap getString (content doc))
              assertEqual "cursor should be at (1,1)" (1, 1) (cursor doc)
              assertEqual "depth should be 1" 1 (depth doc)
    , testCase "delete characters" $
        let doc = foldl updateDoc initDoc [Key 'a', Key 'b', Key 'c', Delete, Delete]
            expectedContent = S.singleton (bufferFromString "a")
         in do
              assertEqual
                "content should be 'a'"
                (fmap getString expectedContent)
                (fmap getString (content doc))
              assertEqual "cursor should be at (0,1)" (0, 1) (cursor doc)
    ]
