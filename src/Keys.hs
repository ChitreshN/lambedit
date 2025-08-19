module Keys where

data Key = ArrowUp | ArrowDown | ArrowLeft | ArrowRight | Delete | Key Char | NewLine

getKey :: IO Key
getKey = do
    c <- getChar
    case c of
        '\ESC' -> do
            seq1 <- getChar
            seq2 <- getChar
            case [seq1, seq2] of
                "[A" -> return ArrowUp
                "[B" -> return ArrowDown
                "[D" -> return ArrowLeft
                "[C" -> return ArrowRight
                _    -> error "Invalid escape sequence"
        '\DEL' -> return Delete
        '\n'   -> return NewLine
        '\r'   -> return NewLine
        _      -> return (Key c)
