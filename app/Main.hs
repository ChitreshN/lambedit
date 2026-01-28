module Main where

import System.Console.ANSI
import System.IO
import System.Environment (getArgs)
import Renderer.Renderer
import Renderer.ViewPort
import Doc
import FileSystem.LoadFile (loadFile)

main :: IO ()
main = do
    args <- getArgs
    case args of 
        [] -> runEditor initDoc
        [name] -> do
          lfile <- loadFile name
          let doc = docWithContent lfile
          runEditor doc
        _ -> putStrLn "Usage: lambedit [file]"
    cleanup

runEditor :: Doc -> IO ()
runEditor doc = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hSetBuffering stdout NoBuffering
    s <- getTerminalSize
    let (maxr, maxc) = case s of
          Just (h, w) -> (h - 2, w - 2) -- if we subtract 1 instead, wierd behavior happens TODO: investigate
          Nothing -> (24, 80) -- fallback
    hideCursor
    clearScreen
    renderLoopWithViewPort doc (ViewPort 0 maxr True)
    cleanup

cleanup :: IO ()
cleanup = do
    showCursor
    setSGR [Reset]
    clearScreen
    setCursorPosition 0 0
    hSetEcho stdin True
