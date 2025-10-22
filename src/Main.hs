module Main where

import System.Console.ANSI
import System.IO
import Renderer.Renderer
import Renderer.ViewPort
import Doc

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hSetBuffering stdout NoBuffering
    hideCursor
    clearScreen
    renderLoopWithViewPort initDoc (ViewPort 0 10)
    cleanup

cleanup :: IO ()
cleanup = do
    showCursor
    setSGR [Reset]
    clearScreen
    setCursorPosition 0 0
    hSetEcho stdin True
