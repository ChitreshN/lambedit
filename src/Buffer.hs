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

insertAt :: Int -> Char -> String -> Buffer
insertAt idx c s = splitString s idx `insert` c

deleteAt :: Int -> String -> Buffer
deleteAt idx s = delete $ splitString s idx

getString :: Int -> Buffer -> String
getString c (Buffer b a) =
  if c <= length b + length a
    then b ++ a
    else b ++ a ++ replicate (c - length b - length a) ' '

printBuffer :: Buffer -> IO ()
printBuffer buf = putStrLn $ getString 0 buf

data Doc = Doc
  { buffer :: Buffer
  , cursor :: (Int, Int)
  , depth  :: Int
  }

-- doc state -> input -> new doc state
-- render loop should ideally have just the doc state as input, it should get key and update doc state
-- the update function should be pure
