module ClassicalMechanics.DoublePendulum where

import Prelude
import Effect (Effect)
import ClassicalMechanics.System as System
import ClassicalMechanics.Helper ( modByFloat )
import ClassicalMechanics.Debug as Debug
import Data.Number ( cos, sin, pi )

import CanvasGeometry.Element as Element
import CanvasGeometry.Point as Point
import CanvasGeometry.Color as Color

type Point = Point.Point
type Element = Element.Element

type Config =
  { gravity :: Number
  , radiusR :: Number
  , radiusS :: Number
  , massR :: Number
  , massS :: Number
  }

type State =
  { time :: Number
  -- angle
  , positionR :: Number
  , positionS :: Number
  -- angular velocity
  , velocityR :: Number
  , velocityS :: Number
  }


microFlow :: Config -> System.MicroFlow State
microFlow { massR, massS, radiusR, radiusS, gravity } { time, positionR, positionS, velocityR, velocityS } epsilon =
  let -- TODO: done
      rs = radiusR*radiusS -- DONE
      
      u = cos (positionR - positionS) -- DONE
      v = sin (positionR - positionS) -- DONE

      cosR = cos positionR -- DONE
      cosS = cos positionS -- DONE


      x = -velocityS*velocityS * rs * v - gravity*massR*radiusR*cosR
      y =  velocityR*velocityR * rs * v - gravity      *radiusS*cosS

      a = massR*radiusR*radiusR
      b = rs*u
      c = b
      d = radiusS*radiusS

      det = 1.0/(a*d - b*c) -- DONE

      a' =   det*d
      b' = - det*b
      c' = - det*c
      d' =   det*a


      accelerationR = a' * x + c' * y -- DONE
      accelerationS = b' * x + d' * y -- DONE
   in
   { time: time + epsilon 
   , positionR: (positionR + epsilon * velocityR) # modByFloat (2.0*pi)
   , positionS: (positionS + epsilon * velocityS) # modByFloat (2.0*pi)
   , velocityR: velocityR + epsilon * accelerationR
   , velocityS: velocityS + epsilon * accelerationS
   }

-- ===Charts===
stateToCartesianPosition :: Config -> State -> { cartesianPositionR :: Point, cartesianPositionS :: Point }
stateToCartesianPosition config state =
  let cartesianPositionRx = config.radiusR * cos state.positionR
      cartesianPositionRy = config.radiusR * sin state.positionR in
  { cartesianPositionR:
    { x: cartesianPositionRx, y: cartesianPositionRy }
  , cartesianPositionS:
    { x: cartesianPositionRx + config.radiusS * cos state.positionS, y: cartesianPositionRy + config.radiusS * sin state.positionS }
  }


view :: Config -> State -> Element
view config state =
  let 
      { cartesianPositionR, cartesianPositionS } = stateToCartesianPosition config state
      h = 10.0 -- scaling constant... shouldn't really be here
  in
  Element.circle Color.black cartesianPositionR (h*2.0) <>
  Element.circle (Color.rgba 0 0 0 0.1) Point.origin (h*config.radiusR) <>
  Element.lineSegment Color.black Point.origin cartesianPositionR <>
  Element.circle Color.green cartesianPositionS (h*2.0) <>
  Element.circle (Color.rgba 1 0 0 0.1) cartesianPositionR (h*config.radiusS) <>
  Element.lineSegment Color.green cartesianPositionR cartesianPositionS

data Msg =
    NewFrameRequested Number
  | ResetOfStateWasRequested State

stepSize :: System.TinyTinyNumber
stepSize = 0.001

update :: Config -> Msg -> State -> Effect State
update config msg state =
  case msg of
    NewFrameRequested frameDurationInMiliseconds -> do
      -- We need to convert miliseconds to seconds.
      pure (System.integrate (microFlow config) (frameDurationInMiliseconds / 1000.0) state stepSize)

    ResetOfStateWasRequested newState ->
      pure newState
