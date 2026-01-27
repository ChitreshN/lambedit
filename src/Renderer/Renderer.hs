module Renderer.Renderer (renderLoopWithViewPort, renderLoop) where

import Doc
import Keys
import Renderer.ViewPort
import System.Console.ANSI

renderLoop :: Doc -> IO ()
renderLoop d = do
  s <- getTerminalSize
  let (maxr, maxc) = case s of
        Just (h, w) -> (h - 1, w - 1)
        Nothing -> (24, 80) -- fallback
  let (r, c) = cursor d
  setCursorPosition r 0
  clearLine
  printLine d r
  setCursorPosition r c
  showCursor

  key <- getKey
  let newDoc = updateDoc d key

  let (r', c') = cursor newDoc
      (newr, newc) = (max 0 (min maxr r'), max 0 (min maxc c'))
      updatedDoc = newDoc{cursor = (newr, newc)}

  renderLoop updatedDoc

renderLoopWithViewPortRe :: Doc -> ViewPort -> IO ()
renderLoopWithViewPortRe d v = do
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
        | x < top v = ViewPort (top v - 1) (bottom v - 1) True
        | x > bottom v = ViewPort (top v + 1) (bottom v + 1) True
        | otherwise = ViewPort (top v) (bottom v) False

  renderLoopWithViewPortRe newDoc newViewPort

renderLoopWithViewPortLine :: Doc -> ViewPort -> IO ()
renderLoopWithViewPortLine d v = do
  let renderDoc = docToViewPort d v
  let (r, c) = renderCursor renderDoc
  setCursorPosition r 0
  clearLine
  printLine d r
  setCursorPosition r c
  showCursor

  key <- getKey
  let newDoc = updateDoc d key
  let (x, _) = cursor newDoc
      newViewPort
        | x < top v = ViewPort (top v - 1) (bottom v - 1) True
        | x > bottom v = ViewPort (top v + 1) (bottom v + 1) True
        | otherwise = ViewPort (top v) (bottom v) False

  renderLoopWithViewPortLine newDoc newViewPort

renderLoopWithViewPort :: Doc -> ViewPort -> IO ()
renderLoopWithViewPort d v =
  if changed v
    then renderLoopWithViewPortRe d v
    else renderLoopWithViewPortLine d v

-- add state to the render loop?? so that we can keep track of when to render
-- the entire doc or just render the line thats currently being edited (this is
-- for the view port)

-- better way to render
