module Tests.Renderer.ViewPortTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Renderer.ViewPort
import Doc
import qualified Buffer as B
import qualified Data.Sequence as S

-- Helper to create a buffer from a string
bufferFromString :: String -> B.Buffer
bufferFromString s = B.Buffer { B.before = S.fromList s, B.after = S.empty, B.lineEnd = length s }

-- Helper to create a doc from a list of strings
docFromStrings :: [String] -> (Int, Int) -> Doc
docFromStrings ss cur = Doc { content = S.fromList (map bufferFromString ss), cursor = cur, depth = length ss - 1 }

tests :: TestTree
tests = testGroup "ViewPort Tests" [
    docToViewPortTests
    ]

docToViewPortTests :: TestTree
docToViewPortTests = testGroup "docToViewPort" [
    testCase "doc fits completely in viewport" $
        let d = docFromStrings ["line1", "line2", "line3"] (0,0)
            v = ViewPort 0 2
            rd = docToViewPort d v
        in do
            assertEqual "render content should be the same" (fmap B.getString (content d)) (fmap B.getString (rendercontent rd))
            assertEqual "render cursor should be the same" (cursor d) (renderCursor rd),

    testCase "doc is larger than viewport, cursor inside" $
        let d = docFromStrings ["line1", "line2", "line3", "line4", "line5"] (2,1)
            v = ViewPort 1 3
            rd = docToViewPort d v
            expectedContent = S.fromList [bufferFromString "line2", bufferFromString "line3", bufferFromString "line4"]
        in do
            assertEqual "render content should be sliced" (fmap B.getString expectedContent) (fmap B.getString (rendercontent rd))
            assertEqual "render cursor should be relative" (1,1) (renderCursor rd),

    testCase "cursor is above viewport" $
        let d = docFromStrings ["line1", "line2", "line3", "line4", "line5"] (0,1)
            v = ViewPort 2 4
            rd = docToViewPort d v
            expectedContent = S.fromList [bufferFromString "line3", bufferFromString "line4", bufferFromString "line5"]
        in do
            assertEqual "render content should be sliced" (fmap B.getString expectedContent) (fmap B.getString (rendercontent rd))
            assertEqual "render cursor should be relative to top" (0,1) (renderCursor rd),

    testCase "cursor is below viewport" $
        let d = docFromStrings ["line1", "line2", "line3", "line4", "line5"] (4,1)
            v = ViewPort 0 2
            rd = docToViewPort d v
            expectedContent = S.fromList [bufferFromString "line1", bufferFromString "line2", bufferFromString "line3"]
        in do
            assertEqual "render content should be sliced" (fmap B.getString expectedContent) (fmap B.getString (rendercontent rd))
            assertEqual "render cursor should be relative to top" (2,1) (renderCursor rd)
    ]

