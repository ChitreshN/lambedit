module Main where

import System.Console.ANSI
import System.IO
import Renderer 
import Doc

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hSetBuffering stdout NoBuffering
    hideCursor
    clearScreen
    renderLoop initDoc
    cleanup

cleanup :: IO ()
cleanup = do
    showCursor
    setSGR [Reset]
    clearScreen
    setCursorPosition 0 0
    hSetEcho stdin True
