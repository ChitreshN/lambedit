module Renderer.ViewPort (
  ViewPort (..),
  RenderDoc (rendercontent, renderCursor),
  docToViewPort,
  printRenderDoc,
  printViewportLine,
) where

import Buffer
import Data.Foldable
import qualified Data.Sequence as S
import Doc
import Utils.ListUtils

data ViewPort = ViewPort
  { top :: Int
  , bottom :: Int
  , changed :: Bool
  }
  deriving (Show)

data RenderDoc = RenderDoc
  { rendercontent :: S.Seq Buffer
  , renderCursor :: (Int, Int)
  }

docToViewPort :: Doc -> ViewPort -> RenderDoc
docToViewPort d v = docToRender
 where
  (x, y) = cursor d
  (newx, newy)
    | x < top v = (top v, y)
    | x > bottom v = (bottom v, y)
    | otherwise = (x, y)

  contentToRender = sliceSeq (content d) (top v) (bottom v)
  docToRender =
    RenderDoc
      { rendercontent = contentToRender
      , renderCursor = (newx - top v, newy)
      }

printRenderDoc :: RenderDoc -> IO ()
printRenderDoc r = printList (toList (rendercontent r))

printViewportLine :: RenderDoc -> Int -> IO ()
printViewportLine r i = forM_ (S.lookup i (rendercontent r)) printBuffer
