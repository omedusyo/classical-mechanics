module Store ( Store, make, send, getModel ) where

import Prelude
import Effect (Effect)

data Store :: Type -> Type
data Store msg

foreign import make :: forall msg model . (Store msg -> msg -> model -> Effect model) -> model -> Effect (Store msg)

foreign import send :: forall msg . msg -> Store msg -> Effect Unit

foreign import getModel :: forall msg model . Store msg -> Effect model
