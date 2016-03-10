module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Array ((..), snoc)
import Data.Foldable (fold, foldMap, maximum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just), maybe)
import Data.Monoid (mempty)
import Data.Ord (max)

import Graphics.Canvas (getCanvasElementById, getContext2D, Context2D, Canvas)
import Graphics.Drawing (Drawing, scale, translate)
import Graphics.Pixel (enemy, ship, bullet, renderClean)

import Signal
import Signal.Time (every)
import Signal.Keyboard

type Player = Int
type Bullet = { x :: Int, y :: Int }
type State = { player :: Player, bullets :: Array Bullet, cooldown :: Int }
type Input = { movement :: Int, fire :: Boolean }

initialState :: State
initialState = {player: 46, bullets: [], cooldown: 0}

newBullet :: Int -> Bullet
newBullet x = {x, y: 50}

moveBullet :: Bullet -> Bullet
moveBullet b = b { y = b.y - 1}

step :: Input -> State -> State
step {movement, fire} {player, bullets, cooldown} =
  let newPlayer = player + movement
      newBullets =
        map moveBullet bullets ++ if firing
                                  then [newBullet (player + 4)]
                                  else []
      firing = fire && cooldown == 0
      newCooldown = if firing
                    then 25
                    else max 0 (cooldown - 1)

  in { player: newPlayer
     , bullets: newBullets
     , cooldown: newCooldown
     }

renderState :: State -> Drawing
renderState {player, bullets} =
  let enemies = foldMap renderEnemy (1..5)
      player' = translate (toNumber player) 50.0 ship
      bullets' = foldMap renderBullet bullets
      renderBullet {x, y} = translate (toNumber x) (toNumber y) bullet
      renderEnemy x = translate (toNumber x * 15.0) 0.0 enemy
  in enemies <> bullets' <> player'

input :: Boolean -> Boolean -> Boolean -> {movement :: Int, fire :: Boolean}
input true false fire  = {movement: -1, fire}
input false false fire = {movement:  0, fire}
input true true fire   = {movement:  0, fire}
input false true fire  = {movement:  1, fire}

view :: forall e. Context2D -> State -> Eff (canvas :: Canvas | e) Unit
view ctx state =
  renderClean ctx $
    translate 150.0 150.0 $
      scale 5.0 5.0 $
        renderState state

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  rArrow <- rightArrow
  lArrow <- leftArrow
  sbar <- spacebar

  let inputs = sampleOn (every 20.0) (input <~ lArrow ~ rArrow ~ sbar)
      states = foldp step initialState inputs
  runSignal $ view ctx <~ states
