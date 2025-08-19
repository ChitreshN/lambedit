module Renderer where

import Control.Concurrent (threadDelay)
import Keys
import System.Console.ANSI
import Doc

renderLoop :: Doc -> IO ()
renderLoop d = do
  s <- getTerminalSize
  let (maxr, maxc) = case s of
        Just (h, w) -> (h - 1, w - 1)
        Nothing -> (24, 80) -- fallback

  setCursorPosition 0 0
  clearFromCursorToScreenEnd
  printDoc d
  let (r, c) = cursor d
  setCursorPosition r c
  showCursor

  key <- getKey
  let newDoc = updateDoc d key

  let (r', c') = cursor newDoc
      (newr, newc) = (max 0 (min maxr r'), max 0 (min maxc c'))
      updatedDoc = newDoc {cursor = (newr, newc)}

  threadDelay 33333
  renderLoop updatedDoc


-- TODO: diff previous and current doc and only render the differences
