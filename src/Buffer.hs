module Buffer where

data Buffer = Buffer
  { before :: String
  , after :: String
  }
  deriving (Show)

splitString :: String -> Int -> Buffer
splitString s idx =
  let
    (b, a) =
      ( if idx < length s
          then splitAt idx s
          else (s, replicate (idx - length s) ' ')
      )
   in
    Buffer b a

insert :: Buffer -> Char -> Buffer
insert (Buffer b a) s = Buffer (b ++ [s]) a

delete :: Buffer -> Buffer
delete (Buffer b "") = Buffer b ""
delete (Buffer b a) = Buffer b (tail a)

insertAt :: Int -> Char -> Buffer -> Buffer
insertAt idx c buf = nbuf
 where
  nbuf =
    let (b, a) = (before buf, after buf) in
    if idx <= length (before buf)
      then
        ( let (bc, ac) = splitAt idx b
           in insert (Buffer bc (ac ++ a)) c
        )
      else
        ( let (bc, ac) = splitAt idx a
           in insert (Buffer (b ++ bc) ac) c
        )

deleteAt :: Int -> Buffer -> Buffer
deleteAt idx buf = nbuf
  where
    nbuf =
      let (b, a) = (before buf, after buf) in
      if idx <= length b
        then
          ( let (bc, ac) = splitAt idx b
             in delete $ Buffer (bc ++ a) ac
          )
        else
          ( let (bc, ac) = splitAt (idx - length b) a
             in delete $ Buffer b (ac ++ bc)
          )

getString :: Buffer -> String
getString (Buffer b a) = b ++ a

printBuffer :: Buffer -> IO ()
printBuffer buf = putStrLn $ getString buf

printList :: [Buffer] -> IO ()
printList [] = return ()
printList (x : xs) = do
  printBuffer x
  printList xs

-- doc state -> input -> new doc state
-- render loop should ideally have just the doc state as input, it should get key and update doc state
-- the update function should be pure
