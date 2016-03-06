module Main where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Graphics.Drawing
import Graphics.Canvas (getCanvasElementById, getContext2D)

main = do
  Just c <- getCanvasElementById "canvas"
  ctx <- getContext2D c
  pure unit
