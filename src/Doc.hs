module Doc (Doc (..), initDoc, updateDoc, printDoc, docWithContent) where

import Buffer
import Data.Foldable
import qualified Data.Sequence as S
import Keys
import Utils.ListUtils

data Doc = Doc
  { content :: S.Seq Buffer
  , cursor :: (Int, Int)
  , depth :: Int
  }
  deriving (Show)

initDoc :: Doc
initDoc = Doc emptyBuf (0, 0) 0

docWithContent :: S.Seq Buffer -> Doc
docWithContent c = Doc c (0, 0) (S.length c)

updateDoc :: Doc -> Key -> Doc
updateDoc d k = case k of
  ArrowUp -> changeCursorBy d (-1) 0
  ArrowDown -> changeCursorBy d 1 0
  ArrowLeft -> changeCursorBy d 0 (-1)
  ArrowRight -> changeCursorBy d 0 1
  NewLine -> setCursorToStartofNextLine d
  Key x -> insertChar d x
  Delete -> deleteChar d

changeCursorBy :: Doc -> Int -> Int -> Doc
changeCursorBy d r c = d{cursor = newCursor}
 where
  (x, y) = cursor d
  de = depth d
  x' = min de (max 0 (x + r))
  le =
    lineEnd
      ( case S.lookup x' (content d) of
          Just b -> b
          _ -> Buffer{lineEnd = 0, before = S.empty, after = S.empty}
      )
  newCursor = (x', min le (y + c))

setCursorToStartofNextLine :: Doc -> Doc
setCursorToStartofNextLine d =
  Doc
    { cursor = (x + 1, 0)
    , content = newcontent
    , depth = depth d + 1
    }
 where
  (x, _) = cursor d
  newcontent =
    S.insertAt
      (x + 1)
      (Buffer{lineEnd = 0, before = S.empty, after = S.empty})
      (content d)

insertChar :: Doc -> Char -> Doc
insertChar d k =
  d
    { content = replaceAt x (insertAt y k (S.index contentPrev x)) contentPrev
    , cursor = newCursor
    , depth = max (depth d) x
    }
 where
  (x, y) = cursor d
  newCursor = (x, y + 1)
  contentPrev = case S.lookup x (content d) of
    Just _ -> content d
    Nothing ->
      S.insertAt
        x
        (Buffer{lineEnd = 0, before = S.empty, after = S.empty})
        (content d)

deleteChar :: Doc -> Doc
deleteChar d =
  d
    { content = replaceAt x (deleteAt y (S.index (content d) x)) (content d)
    , cursor = newCursor
    , depth = max (depth d) x
    }
 where
  (x, y) = cursor d
  newCursor = (x, min y (lineEnd (S.index (content d) x)))

printDoc :: Doc -> IO ()
printDoc doc = printList (take d (toList c))
 where
  d = depth doc + 1
  c = content doc
