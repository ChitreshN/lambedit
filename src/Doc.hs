module Doc where

import Buffer
import Keys

data Doc = Doc
  { content :: String
  , cursor :: (Int, Int)
  , depth :: Int
  }

initDoc :: Doc
initDoc = Doc "" (0, 0) 0

updateDoc :: Doc -> Key -> Doc
updateDoc d k = case k of
  ArrowUp -> changeCursorBy d (-1) 0
  ArrowDown -> changeCursorBy d 1 0
  ArrowLeft -> changeCursorBy d 0 (-1)
  ArrowRight -> changeCursorBy d 0 1
  Key x -> insertChar d x
  Delete -> d

changeCursorBy :: Doc -> Int -> Int -> Doc
changeCursorBy d r c = d{cursor = (x + r, y + c)}
 where
  (x, y) = cursor d

insertChar :: Doc -> Char -> Doc
insertChar d k =
  d
    { content = getString y $ insertAt y k (content d)
    , cursor = newCursor -- TODO: fix this
    }
 where
  (x, y) = cursor d
  newCursor = (x, y + 1)

deleteChar :: Doc -> Doc
deleteChar d =
  d
    { content = getString y $ deleteAt y (content d)
    , cursor = newCursor
    }
 where
  (x, y) = cursor d
  newCursor = (x, y - 1)
