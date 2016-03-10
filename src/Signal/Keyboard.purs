module Signal.Keyboard where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Signal (Signal, runSignal)
import Signal.DOM (keyPressed)

rightArrow :: forall e. Eff (dom :: DOM | e) (Signal Boolean)
rightArrow = keyPressed 39

leftArrow :: forall e. Eff (dom :: DOM | e) (Signal Boolean)
leftArrow = keyPressed 37

spacebar :: forall e. Eff (dom :: DOM | e) (Signal Boolean)
spacebar = keyPressed 32

logSignal :: forall a e. Signal a -> Eff (console :: CONSOLE | e) Unit
logSignal s = runSignal $ Control.Monad.Eff.Console.Unsafe.logAny <$> s

hole :: forall a. a
hole = Unsafe.Coerce.unsafeCoerce unit
