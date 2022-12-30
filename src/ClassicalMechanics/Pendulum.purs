module ClassicalMechanics.Pendulum
  ( Config
  , State
  , Msg(..)

  , config0
  , microFlow
  , update

  , view
  ) where

import Prelude
import Effect (Effect)
import ClassicalMechanics.System as System
import ClassicalMechanics.Helper ( modByFloat )
import Data.Number ( cos, sin, pi )

import CanvasGeometry.Element as Element
import CanvasGeometry.Point as Point
import CanvasGeometry.Color as Color

type Point = Point.Point
type Element = Element.Element


type Config = { gravity :: Number, radius :: Number }

config0 :: { radius :: Number } -> Config
config0 { radius } = { gravity: 9.8, radius: radius }

type State = { time :: Number, position :: Number, velocity :: Number }

microFlow :: Config -> System.MicroFlow State
microFlow config state epsilon =
  state
    { time = state.time + epsilon 
    , position = (state.position + epsilon * state.velocity) # modByFloat (2.0*pi) -- TODO: Will this work?
    , velocity = state.velocity + epsilon * (- config.gravity / config.radius) * cos state.position
    }

-- ===Coord Transformation===
stateToCartesianPosition :: Config -> State -> Point
stateToCartesianPosition { radius } { position } =
  { x: radius * cos position, y: radius * sin position }

-- ===Rendering===
view :: Config -> State -> Element
view config state =
  let 
      pendulumCenter :: Point
      pendulumCenter = stateToCartesianPosition config state
  in
  Element.circle Color.black pendulumCenter 20.0 <> -- 20 is in pixels
  Element.lineSegment Color.black Point.origin pendulumCenter

data Msg =
    NewFrameRequested Number
  | ResetOfStateWasRequested State

update :: Config -> Msg -> State -> Effect State
update config msg state =
  case msg of
    NewFrameRequested frameDurationInMiliseconds -> do
      -- We need to convert miliseconds to seconds.
      pure (System.integrate (microFlow config) (frameDurationInMiliseconds / 1000.0) state 0.001)

    ResetOfStateWasRequested newState ->
      pure newState
