module Buffer where

data Buffer = Buffer
  { before :: String
  , after :: String
  }

splitString :: String -> Int -> Buffer
splitString s idx =
  let
    b = take idx s
    a = drop idx s
    in Buffer b a

insert :: Buffer -> Char -> Buffer
insert (Buffer b a) s = Buffer (b ++ [s]) a

delete :: Buffer -> Buffer
delete (Buffer b a) = Buffer b (tail a)

insertAt :: Int -> Char -> String -> Buffer
insertAt idx c s = splitString s idx `insert` c

printBuffer :: Buffer -> IO ()
printBuffer (Buffer b a) = putStrLn $ b ++ a
