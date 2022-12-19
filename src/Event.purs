module Event
  ( requestAnimationFrame
  ) where

import Prelude
import Effect (Effect)


foreign import foreignRequestAnimationFrame :: (Number -> Effect Unit) -> Effect Unit

requestAnimationFrame :: (Number -> Effect Unit) -> Effect Unit
requestAnimationFrame = foreignRequestAnimationFrame
