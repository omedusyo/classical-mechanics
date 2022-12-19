module ClassicalMechanics.Pendulum ( Config, config0, State, microFlow, render, update, Msg(..) ) where

import Prelude
import Effect (Effect)
import Effect.Console ( log )
import ClassicalMechanics.System as System
import Data.Number ( cos, sin, pi )
import Canvas as Canvas


type Config = { gravity :: Number, radius :: Number }

config0 :: { radius :: Number } -> Config
config0 { radius } = { gravity: 9.8, radius: radius }

type State = { time :: Number, position :: Number, velocity :: Number }

microFlow :: Config -> System.MicroFlow State
microFlow config state epsilon =
  state
    { time = state.time + epsilon
    , position = state.position + epsilon * state.velocity
    , velocity = state.velocity + epsilon * (- config.gravity / config.radius) * cos state.position
    }

-- ===Coord Transformation===
coordPosition :: Config -> State -> { x :: Number, y :: Number }
coordPosition { radius } { position } =
  { x: radius * cos position, y: radius * sin position }

-- ===Rendering===

render :: (Canvas.CanvasRef -> Effect Unit) -> ({ x :: Number, y :: Number } -> { canvasX :: Number, canvasY :: Number }) -> Config -> State -> Canvas.CanvasRef -> Effect Unit
render clear fromCartesianToCanvas config state canvasRef = do
  canvasRef # clear
  let pendulumCenterCanvasCoord = fromCartesianToCanvas (coordPosition config state)
  let cartesianOrigin = fromCartesianToCanvas { x : 0.0, y : 0.0 }

  line <- Canvas.makePath2D
  line # Canvas.moveTo cartesianOrigin.canvasX cartesianOrigin.canvasY
  line # Canvas.lineTo pendulumCenterCanvasCoord.canvasX pendulumCenterCanvasCoord.canvasY
  canvasRef # Canvas.stroke line

  circle <- Canvas.makePath2D
  circle # Canvas.arc pendulumCenterCanvasCoord.canvasX pendulumCenterCanvasCoord.canvasY 20.0 0.0 (2.0*pi) -- 20 is in pixels
  canvasRef # Canvas.stroke circle

  pure unit

data Msg =
    NewFrameRequested Number
  | ResetOfStateWasRequested State

update :: Config -> Msg -> State -> Effect State
update config msg state =
  case msg of
    NewFrameRequested frameDurationInMiliseconds -> do
      -- We need to convert miliseconds to seconds.
      -- log (show frameDurationInMiliseconds)
      -- log (show state)
      pure (System.integrate (microFlow config) (frameDurationInMiliseconds / 1000.0) state 0.001)

    ResetOfStateWasRequested newState ->
      pure newState
