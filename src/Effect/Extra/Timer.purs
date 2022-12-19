module Effect.Extra.Timer where

import Prelude

import Effect.Ref as Ref
import Effect (Effect)
import Duration ( Duration )
import Effect.Timer as Timer

intervalLoop :: forall s . Duration -> s -> (s -> Effect s) -> Effect Unit
intervalLoop duration initState nextState = do
  r <- Ref.new initState
  _ <- Timer.setInterval duration (do
    state <- Ref.read r
    newState <- nextState state
    r # Ref.write newState
  )
  pure unit
