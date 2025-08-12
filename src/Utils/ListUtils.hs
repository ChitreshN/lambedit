module Utils.ListUtils where

import Buffer

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 x (_ : xs) = x : xs
replaceAt n x (y : ys) = y : replaceAt (n - 1) x ys

infiniteEmpty :: [Buffer]
infiniteEmpty = Buffer "" "" : infiniteEmpty
