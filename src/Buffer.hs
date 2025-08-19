module Buffer where

import qualified Data.Sequence as Seq
import           Data.Sequence ((|>), (><))
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

data Buffer = Buffer
  { before :: Seq.Seq Char
  , after ::  Seq.Seq Char
  , lineEnd :: Int
  }
  deriving (Show)

insert :: Buffer -> Char -> Buffer
insert (Buffer b a le) s = Buffer (b |> s) a (le + 1)

delete :: Buffer -> Buffer
delete (Buffer Seq.Empty a le) = Buffer Seq.Empty a le
delete (Buffer b a le) = Buffer (Seq.take (Seq.length b - 1) b) a (le - 1)

insertAt :: Int -> Char -> Buffer -> Buffer
insertAt idx c buf = nbuf
 where
  nbuf =
    let (b, a, le) = (before buf, after buf, lineEnd buf) in
    if idx <= length (before buf)
      then
        ( let (bc, ac) = Seq.splitAt idx b
           in insert (Buffer bc (ac >< a) le) c
        )
      else
        ( let (bc, ac) = Seq.splitAt (idx - length b) a
           in insert (Buffer (b >< bc) ac le) c
        )

deleteAt :: Int -> Buffer -> Buffer
deleteAt idx buf = nbuf
  where
    nbuf =
      let (b, a, le) = (before buf, after buf, lineEnd buf) in
      if idx <= length b
        then
          ( let (bc, ac) = Seq.splitAt idx b
             in delete $ Buffer (bc >< a) ac le
          )
        else
          ( let (bc, ac) = Seq.splitAt (idx - length b) a
             in delete $ Buffer b (ac >< bc) le
          )

getString :: Buffer -> String
getString (Buffer b a _) = F.toList $ b >< a

printBuffer :: Buffer -> IO ()
printBuffer buf = T.IO.putStrLn $ T.pack (getString buf)

printList :: [Buffer] -> IO ()
printList [] = return ()
printList (x : xs) = do
  printBuffer x
  printList xs

-- doc state -> input -> new doc state
-- render loop should ideally have just the doc state as input, it should get key and update doc state
-- the update function should be pure
