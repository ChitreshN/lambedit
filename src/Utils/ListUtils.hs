module Utils.ListUtils where

import Buffer
import qualified Data.Sequence as Seq

replaceAt :: Int -> a -> Seq.Seq a -> Seq.Seq a
replaceAt = Seq.update

emptyBuf :: Seq.Seq Buffer
emptyBuf = Seq.Empty

spliceSeq :: Seq.Seq a -> Int -> Int -> Seq.Seq a
spliceSeq s h t = final
  where
    leftToT = Seq.take t s
    final = Seq.drop h leftToT
