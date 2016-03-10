module Signal.Keyboard where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Signal (Signal)
import Signal.DOM (keyPressed)

rightArrow :: forall e. Eff (dom :: DOM | e) (Signal Boolean)
rightArrow = keyPressed 39

leftArrow :: forall e. Eff (dom :: DOM | e) (Signal Boolean)
leftArrow = keyPressed 37
