module Renderer where

import System.Console.ANSI
import System.IO
import Control.Concurrent (threadDelay)
import Buffer

renderLoop :: Buffer -> IO ()
renderLoop b = do
  hasInput <- hReady stdin
  key <- if hasInput then Just <$> getChar else pure Nothing
  newBuffer <- case key of
    Just x -> pure (insert b x)
    Nothing ->  pure b
  setCursorPosition 0 0
  clearFromCursorToScreenEnd
  putStrLn $ before newBuffer ++ after newBuffer
  threadDelay 33333
  renderLoop newBuffer
