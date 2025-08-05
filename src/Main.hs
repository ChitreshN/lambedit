{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Console.ANSI
import System.IO
import Renderer 
import Buffer

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hSetBuffering stdout NoBuffering
    hideCursor
    clearScreen
    renderLoop 0 0 (Buffer "before" " after")
    cleanup

cleanup :: IO ()
cleanup = do
    showCursor
    setSGR [Reset]
    clearScreen
    setCursorPosition 0 0
    hSetEcho stdin True
