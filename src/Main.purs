module Main where

import Prelude

import Data.Array ((..), length, head, zip)
import Data.Foldable (fold, foldMap)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just), maybe)
import Data.Monoid (mempty)
import Data.String as S
import Data.Tuple (uncurry)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Drawing (Shape, scale, translate, render, black, fillColor, filled, rectangle)

pixelArt :: Array String -> Shape
pixelArt lines =
  let grid = S.toCharArray <$> lines
      maxX = maybe 0 S.length (head lines)
      maxY = length lines
      pixelLine y s = foldMap (uncurry (flip pixel y)) (zip (1 .. maxX) s)
      pixel x y ' ' = mempty
      pixel x y _ = pixelShape x y
      pixelShape x y = rectangle (toNumber x) (toNumber y) 1.0 1.0
  in foldMap (uncurry pixelLine) (zip (1 .. maxY) grid)

enemy = fillBlack $ pixelArt $
  [ "   x     x   "
  , "    x   x    "
  , "   xxxxxxx   "
  , "  xx xxx xx  "
  , " xxxxxxxxxxx "
  , " x xxxxxxx x "
  , " x x     x x "
  , "    xx xx    "
  ]

ship = fillBlack $ pixelArt $
  [ "     x     "
  , "    xxx    "
  , "   xxxxx   "
  , " xxxxxxxxx "
  , "xxxxxxxxxxx"
  , "xxxxxxxxxxx"
  ]

bullet = fillBlack $ pixelArt $
  [ " x "
  , "xxx"
  ]

fillBlack = filled (fillColor black)

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

  render ctx $
    -- shadow (shadowColor black <> shadowBlur 1.0) $
      translate 150.0 150.0 $
        scale 5.0 5.0 $
          image
