module Utils.ListUtils where

import Buffer
import qualified Data.Sequence as Seq

replaceAt :: Int -> a -> Seq.Seq a -> Seq.Seq a
replaceAt = Seq.update

emptyBuf :: Seq.Seq Buffer
emptyBuf = Seq.Empty
