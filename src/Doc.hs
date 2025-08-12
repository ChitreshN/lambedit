module Doc where

import Buffer
import Keys
import Utils.ListUtils

data Doc = Doc
  { content :: [Buffer]
  , cursor :: (Int, Int)
  , depth :: Int
  }

initDoc :: Doc
initDoc = Doc infiniteEmpty (0, 0) 0

updateDoc :: Doc -> Key -> Doc
updateDoc d k = case k of
  ArrowUp -> changeCursorBy d (-1) 0
  ArrowDown -> changeCursorBy d 1 0
  ArrowLeft -> changeCursorBy d 0 (-1)
  ArrowRight -> changeCursorBy d 0 1
  Key x -> insertChar d x
  Delete -> deleteChar d

changeCursorBy :: Doc -> Int -> Int -> Doc
changeCursorBy d r c = d{cursor = (x + r, y + c)}
 where
  (x, y) = cursor d

insertChar :: Doc -> Char -> Doc
insertChar d k =
  d
    { content = replaceAt x (insertAt y k (content d !! x)) (content d)
    , cursor = newCursor
    , depth = max (depth d) x
    }
 where
  (x, y) = cursor d
  newCursor = (x, y + 1)

deleteChar :: Doc -> Doc
deleteChar d =
  d
    { content = replaceAt x (deleteAt y (content d !! x)) (content d)
    , cursor = newCursor
    , depth = max (depth d) x
    }
 where
  (x, y) = cursor d
  newCursor = (x, y - 1)

printDoc :: Doc -> IO ()
printDoc doc = printList (take d c)
  where
    d = depth doc + 1
    c = content doc
