module Renderer.Renderer where

import Control.Concurrent (threadDelay)
import Renderer.ViewPort
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


{-
- Use a viewport to render the document.
- with this approach, we can potentially implement scrolling as well.
 -}

renderLoopWithViewPort :: Doc -> ViewPort -> IO ()
renderLoopWithViewPort d v = do
  let renderDoc = docToViewPort d v
  setCursorPosition 0 0
  clearFromCursorToScreenEnd
  printRenderDoc renderDoc
  let (r, c) = renderCursor renderDoc
  setCursorPosition r c
  showCursor

  key <- getKey
  let newDoc = updateDoc d key
  let (x, _) = cursor newDoc
      newViewPort
        | x <= top v = ViewPort (top v - 1) (bottom v - 1)
        | x >= bottom v = ViewPort (top v + 1) (bottom v + 1)
        | otherwise = ViewPort (top v) (bottom v)

  threadDelay 33333
  renderLoopWithViewPort newDoc newViewPort
