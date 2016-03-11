module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Array ((..), snoc, filter)
import Data.Foldable (fold, foldMap, maximum, any)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just))
import Data.Monoid (mempty)
import Data.Ord (max)

import Math

import Graphics.Canvas (getCanvasElementById, getContext2D, Context2D, Canvas)
import Graphics.Drawing (Drawing, scale, translate)
import Graphics.Pixel (enemy, ship, bullet, renderClean)

import Signal ((<~), runSignal, foldp, (~), sampleOn)
import Signal.Time (every)
import Signal.Keyboard


















view ctx =
  renderClean ctx $
    translate 50.0 150.0 $
      scale 5.0 5.0 $
        enemy

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  view ctx
