module Main where

import Prelude

import Data.Array ((..))
import Data.Foldable (fold, foldMap)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just), maybe)
import Data.Monoid (mempty)

import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Drawing
import Graphics.Pixel (enemy, ship, bullet, renderClean)

import Signal
import Signal.Time (every)
import Signal.Keyboard (rightArrow, leftArrow)

image =
  let enemies = fold do
        i <- 1..5
        pure (translate (toNumber i * 15.0) 0.0 enemy)
      player = translate 46.0 50.0 ship
      bullets = fold do
        i <- 1..8
        pure (translate 50.0 (6.0 + (toNumber i * 5.0)) bullet)
  in enemies <> bullets <> player

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  renderClean ctx $
    translate 150.0 150.0 $
      scale 5.0 5.0 $
        image
