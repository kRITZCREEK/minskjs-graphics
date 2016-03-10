module Graphics.Pixel where

import Prelude

import Data.Array ((..), length, head, zip)
import Data.Foldable (fold, foldMap)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just), maybe)
import Data.Monoid (mempty)
import Data.String as S
import Data.Tuple (uncurry)
import Graphics.Drawing (Drawing, Shape, translate, render, black, fillColor, filled, rectangle, white)


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

enemy :: Drawing
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

ship :: Drawing
ship = fillBlack $ pixelArt $
  [ "     x     "
  , "    xxx    "
  , "   xxxxx   "
  , " xxxxxxxxx "
  , "xxxxxxxxxxx"
  , "xxxxxxxxxxx"
  ]

bullet :: Drawing
bullet = fillBlack $ pixelArt $
  [ " x "
  , "xxx"
  ]

fillBlack :: Shape -> Drawing
fillBlack = filled (fillColor black)

renderClean ctx s = render ctx $ clear <> s

clear :: Drawing
clear = filled (fillColor white) $ rectangle 0.0 0.0 800.0 800.0
