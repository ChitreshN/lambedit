module Main where

import System.Console.ANSI
import System.IO
import System.Environment (getArgs)
import Renderer.Renderer (renderLoopWithViewPort)
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

runEditor :: Doc -> IO ()
runEditor doc = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hSetBuffering stdout NoBuffering
    hideCursor
    clearScreen
    renderLoopWithViewPort doc (ViewPort 0 10)
    cleanup

cleanup :: IO ()
cleanup = do
    showCursor
    setSGR [Reset]
    clearScreen
    setCursorPosition 0 0
    hSetEcho stdin True
