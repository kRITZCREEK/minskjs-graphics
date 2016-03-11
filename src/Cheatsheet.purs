module CheatSheet where

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

type Player = Int
type Enemy = Int
type Bullet =
  { x :: Int
  , y :: Int
  }
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

initialState :: State
initialState =
  { player: 46
  , enemies: map (15 * _) (1..5)
  , bullets: []
  , cooldown: 0
  }

newBullet :: Int -> Bullet
newBullet x = {x, y: 50}

moveBullet :: Bullet -> Bullet
moveBullet {x, y} = { x, y: y - 1 }

collides :: Enemy -> Bullet -> Boolean
collides e {x, y} = dist y 0 < 10.0 && dist e x < 4.0
  where dist f g = Math.abs (toNumber (f - g))

step :: Input -> State -> State
step {movement, fire} {player, enemies, bullets, cooldown} =
  let newPlayer = player + movement
      newEnemies = filter (\e -> not $ any (collides e) bullets) enemies
      newBullets =
        map moveBullet bullets ++ if firing
                                  then [newBullet (player + 4)]
                                  else []
      firing = fire && cooldown == 0
      newCooldown = if firing
                    then 25
                    else max 0 (cooldown - 1)

  in { player: newPlayer
     , enemies: newEnemies
     , bullets: newBullets
     , cooldown: newCooldown
     }

renderState :: State -> Drawing
renderState {player, bullets, enemies} =
  let enemies' = foldMap renderEnemy enemies
      player' = translate (toNumber player) 50.0 ship
      bullets' = foldMap renderBullet bullets
      renderBullet {x, y} = translate (toNumber x) (toNumber y) bullet
      renderEnemy x = translate (toNumber x) 0.0 enemy
  in enemies' <> bullets' <> player'

input :: Boolean -> Boolean -> Boolean -> {movement :: Int, fire :: Boolean}
input true false fire  = {movement: -1, fire}
input false false fire = {movement:  0, fire}
input true true fire   = {movement:  0, fire}
input false true fire  = {movement:  1, fire}

view :: forall e. Context2D -> State -> Eff (canvas :: Canvas | e) Unit
view ctx state =
  renderClean ctx $
    translate 50.0 150.0 $
      scale 5.0 5.0 $
        renderState state

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  sbar <- spacebar
  lArrow <- leftArrow
  rArrow <- rightArrow

  let inputs = sampleOn (every 20.0) (input <~ lArrow ~ rArrow ~ sbar)
      states = foldp step initialState inputs
  runSignal $ view ctx <~ states
