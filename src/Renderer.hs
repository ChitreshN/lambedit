module Renderer where

import Buffer
import Control.Concurrent (threadDelay)
import Keys
import System.Console.ANSI

renderLoop :: Int -> Int -> Buffer -> IO ()
renderLoop r c b = do
  s <- getTerminalSize
  let (maxr, maxc) = case s of
        Just (h, w) -> (h - 1, w - 1)
        Nothing -> (24, 80) -- fallback
  setCursorPosition 0 0
  clearFromCursorToScreenEnd
  putStrLn $ before b ++ after b
  setCursorPosition r c
  showCursor


  key <- getKey

  let (newBuffer, r', c') = case key of
        Key x -> (insertAt c x (getString c b), r, c + 1)
        ArrowUp -> (b, r - 1, c)
        ArrowDown -> (b, r + 1, c)
        ArrowLeft -> (b, r, c - 1)
        ArrowRight -> (b, r, c + 1)
        Delete -> (deleteAt c (getString c b), r, c - 1)

  let (newr, newc) = (max 0 (min maxr r'), max 0 (min maxc c'))

  threadDelay 33333
  renderLoop newr newc newBuffer
