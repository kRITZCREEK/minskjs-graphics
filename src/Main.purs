module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Array ((..), snoc, filter)
import Data.Foldable (fold, foldMap, maximum, any)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just))
import Data.Monoid (mempty)
import Data.Ord (max)

import Math (abs)

import Graphics.Canvas (getCanvasElementById, getContext2D, Context2D, Canvas)
import Graphics.Drawing (Drawing, scale, translate)
import Graphics.Pixel (enemy, ship, bullet, renderClean)

import Signal ((<~), runSignal, foldp, (~), sampleOn)
import Signal.Time (every)
import Signal.Keyboard












type Player = Int
type Enemy = Int
type Bullet = {x :: Int, y :: Int}

type State =
  { player :: Player
  , enemies :: Array Enemy
  , bullets :: Array Bullet
  , cooldown :: Int
  }

type Input =
  { movement :: Int
  , fire :: Boolean
  }

initialState =
  { player: 46
  , enemies: map (15 * _) (1..5)
  , bullets: []
  , cooldown: 0
  }



combineInput :: Boolean -> Boolean -> Boolean -> Input
combineInput false false fire = {movement: 0, fire}
combineInput true false fire = {movement: negate 1, fire}
combineInput false true fire = {movement: 1, fire}
combineInput true true fire = {movement: 0, fire}

renderState :: State -> Drawing
renderState {player, enemies, bullets} =
  let renderEnemy i = translate (toNumber i) 0.0 enemy
      renderBullet {x, y} = translate (toNumber x) (toNumber y) bullet

      bullets' = foldMap renderBullet bullets
      enemies' = foldMap renderEnemy enemies
      player' = translate (toNumber player) 50.0 ship
  in enemies' <> player' <> bullets'

view ctx state =
  renderClean ctx $
    translate 50.0 150.0 $
      scale 5.0 5.0 $
        renderState state

newBullet x = {x, y: 50}
moveBullet :: Bullet -> Bullet
moveBullet {x, y} = {x, y: y - 1}

collides :: Enemy -> Bullet -> Boolean
collides e {x, y} = dist y 0 < 8.0 && dist e x < 8.0
  where dist f g = abs (toNumber (f - g))

update :: Input -> State -> State
update {movement, fire} {player, enemies, bullets, cooldown} =
  let newPlayer = player + movement
      firing = fire && cooldown == 0
      newCooldown = if firing
                    then 30
                    else max 0 (cooldown - 1)
      newBullets = map moveBullet bullets ++ if firing
                              then [newBullet player]
                              else []
      newEnemies =
        filter (\e -> not $ any (collides e) bullets) enemies
  in
    { player: newPlayer
    , enemies: newEnemies
    , bullets: newBullets
    , cooldown: newCooldown
    }

foreign import sin :: Number -> Number

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  lArr <- leftArrow
  rArr <- rightArrow
  sbar <- spacebar

  let inputs =
        sampleOn (every 20.0) $
          combineInput <~ lArr ~ rArr ~ sbar
      states = foldp update initialState inputs

  runSignal $ view ctx <~ states

  Control.Monad.Eff.Console.log (show (sin Math.pi))
