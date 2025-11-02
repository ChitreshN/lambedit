module Tests.Utils.ListUtilsTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Utils.ListUtils
import qualified Data.Sequence as S

tests :: TestTree
tests = testGroup "ListUtils Tests" [
    replaceAtTests,
    sliceSeqTests,
    emptyBufTests
    ]

replaceAtTests :: TestTree
replaceAtTests = testGroup "replaceAt" [
    testCase "replace at beginning" $
        let s :: S.Seq Int
            s = S.fromList [1, 2, 3]
            newS = replaceAt 0 9 s
        in assertEqual "should be [9,2,3]" (S.fromList [9,2,3]) newS,
    testCase "replace in middle" $
        let s :: S.Seq Int
            s = S.fromList [1, 2, 3]
            newS = replaceAt 1 9 s
        in assertEqual "should be [1,9,3]" (S.fromList [1,9,3]) newS,
    testCase "replace at end" $
        let s :: S.Seq Int
            s = S.fromList [1, 2, 3]
            newS = replaceAt 2 9 s
        in assertEqual "should be [1,2,9]" (S.fromList [1,2,9]) newS
    ]

sliceSeqTests :: TestTree
sliceSeqTests = testGroup "sliceSeq" [
    testCase "slice from beginning" $
        let s :: S.Seq Int
            s = S.fromList [1, 2, 3, 4, 5]
            sliced = sliceSeq s 0 2
        in assertEqual "should be [1,2,3]" (S.fromList [1,2,3]) sliced,
    testCase "slice in middle" $
        let s :: S.Seq Int
            s = S.fromList [1, 2, 3, 4, 5]
            sliced = sliceSeq s 1 3
        in assertEqual "should be [2,3,4]" (S.fromList [2,3,4]) sliced,
    testCase "slice till end" $
        let s :: S.Seq Int
            s = S.fromList [1, 2, 3, 4, 5]
            sliced = sliceSeq s 2 4
        in assertEqual "should be [3,4,5]" (S.fromList [3,4,5]) sliced,
    testCase "slice single element" $
        let s :: S.Seq Int
            s = S.fromList [1, 2, 3, 4, 5]
            sliced = sliceSeq s 2 2
        in assertEqual "should be [3]" (S.fromList [3]) sliced
    ]

emptyBufTests :: TestTree
emptyBufTests = testGroup "emptyBuf" [
    testCase "is empty" $
        assertBool "should be empty" (S.null emptyBuf)
    ]
