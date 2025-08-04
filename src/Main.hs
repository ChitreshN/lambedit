{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Console.ANSI
import System.IO
import Buffer
import Renderer

main :: IO ()
main = do
    -- Configure terminal for raw input
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hSetBuffering stdout NoBuffering
    clearScreen

    renderLoop $ Buffer "before" "after"
