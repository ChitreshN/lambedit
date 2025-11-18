module Buffer where

import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

data Buffer = Buffer
  { buffer :: Seq.Seq Char
  , lineEnd :: Int
  }
  deriving (Show)

insertAt :: Int -> Char -> Buffer -> Buffer
insertAt idx c buf =
  Buffer
    { buffer = Seq.insertAt idx c (buffer buf)
    , lineEnd = lineEnd buf + 1
    }

deleteAt :: Int -> Buffer -> Buffer
deleteAt idx buf =
  Buffer
    { buffer = Seq.deleteAt (idx - 1) (buffer buf)
    , lineEnd = min 0 (lineEnd buf)
    }

getString :: Buffer -> String
getString (Buffer b _) = F.toList b

printBuffer :: Buffer -> IO ()
printBuffer b = T.IO.putStrLn $ T.pack (getString b)

printList :: [Buffer] -> IO ()
printList [] = return ()
printList (x : xs) = do
  printBuffer x
  printList xs
