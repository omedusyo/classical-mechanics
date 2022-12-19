module ClassicalMechanics.System
  ( integrate
  , TinyTinyNumber
  , Duration
  , MicroFlow
  , GlobalFlow
  ) where

import Prelude
import Data.Int ( round )


type TinyTinyNumber = Number

type Duration = Number

type MicroFlow state = state -> TinyTinyNumber -> state

type GlobalFlow state = Duration -> state -> TinyTinyNumber -> state


loop :: forall state . MicroFlow state -> Int -> MicroFlow state
loop microFlow n state epsilon =
  case n of
    0 -> state
    _ -> loop microFlow (n - 1) (microFlow state epsilon) epsilon

integrate :: forall state . MicroFlow state -> GlobalFlow state
integrate microFlow t state epsilon =
  let n = round (t / epsilon)
   in
    loop microFlow n state epsilon
