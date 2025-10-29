module Utils.ListUtils where

import Buffer
import qualified Data.Sequence as Seq

replaceAt :: Int -> a -> Seq.Seq a -> Seq.Seq a
replaceAt = Seq.update

emptyBuf :: Seq.Seq Buffer
emptyBuf = Seq.Empty

sliceSeq :: Seq.Seq a -> Int -> Int -> Seq.Seq a
sliceSeq s h t = final
  where
    leftToT = Seq.take (t+1) s
    final = Seq.drop h leftToT
