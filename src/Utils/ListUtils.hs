module Utils.ListUtils where

import Buffer
import qualified Data.Sequence as Seq

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 x (_ : xs) = x : xs
replaceAt n x (y : ys) = y : replaceAt (n - 1) x ys

infiniteEmpty :: [Buffer]
infiniteEmpty = Buffer Seq.Empty Seq.Empty 0 : infiniteEmpty
