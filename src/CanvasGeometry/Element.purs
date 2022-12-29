module CanvasGeometry.Element where

import Prelude
import CanvasGeometry.Vector as Vector
import CanvasGeometry.Point as Point
import CanvasGeometry.Color as Color
import Effect (Effect)
import Canvas as Canvas
import Data.Number ( pi )
import Data.Traversable ( traverse_ )


-- TODO: Introduce proper attributes
data PrimitiveElement =
    Circle Color.Color Point.Point Number
  | Disk Color.Color Point.Point Number
  | LineSegment Color.Color Point.Point Point.Point

instance showPrimitiveElement :: Show PrimitiveElement where
  show (Circle color center radius) =
    "Circle(center: " <> show center <> ", radius: " <> show radius <> ", color: " <> show color <> ")"
     
  show (Disk color center radius) =
    "Disk(center: " <> show center <> ", radius: " <> show radius <> ", color: " <> show color <> ")"

  show (LineSegment color p0 p1) =
    "LineSegment(p0: " <> show p0 <> ", p1: " <> show p1 <> ", color: " <> show color <> ")"

type Element = Array PrimitiveElement

colorToString :: Color.Color -> String
colorToString (Color.RGBA r g b a) =
  "rgba(" <> show r <> "," <> show g <> "," <> show b <> "," <> show a <> ")"

circle :: Color.Color -> Point.Point -> Number -> Element
circle color center radius = [ Circle color center radius ]

lineSegment :: Color.Color -> Point.Point -> Point.Point -> Element
lineSegment color p0 p1 = [ LineSegment color p0 p1 ]

view :: Element
view =
  let center :: Point.Point
      center = (Point.add Point.origin (Vector.make 15.0 16.0)) in
  circle Color.black center 5.0 <>
  lineSegment Color.black Point.origin center

-- ===Rendering===
cartesianToCanvasCoord :: Canvas.CanvasConfig -> Point.Point -> { canvasX :: Number, canvasY :: Number }
cartesianToCanvasCoord canvas { x, y } =
  -- TODO: This scale should be an open parameter...
  -- 1 meter == 10 pixels
  let scale = 10.0 in
  { canvasX: canvas.width/2.0 + scale * x, canvasY: canvas.height/2.0 - scale * y }


renderPrimitiveElement :: Canvas.CanvasConfig -> Canvas.CanvasRef -> PrimitiveElement -> Effect Unit
renderPrimitiveElement config canvasRef =
  case _ of
    Circle color center radius -> do
      let pixelCoordinatesCenter = cartesianToCanvasCoord config center
      canvasCircle <- Canvas.makePath2D
      -- TODO: What if I wanted to use different units for radius?
      canvasCircle # Canvas.arc pixelCoordinatesCenter.canvasX pixelCoordinatesCenter.canvasY radius 0.0 (2.0*pi)
      canvasRef # Canvas.setStrokeStyle (colorToString color)
      canvasRef # Canvas.stroke canvasCircle

    Disk color center radius -> do
      let pixelCoordinatesCenter = cartesianToCanvasCoord config center
      canvasCircle <- Canvas.makePath2D
      -- TODO: What if I wanted to use different units for radius?
      canvasCircle # Canvas.arc pixelCoordinatesCenter.canvasX pixelCoordinatesCenter.canvasY radius 0.0 (2.0*pi)
      canvasRef # Canvas.setFillStyle (colorToString color)
      canvasRef # Canvas.fill canvasCircle

    LineSegment color p0 p1 -> do
      let pixeclCoordinatesP0 = cartesianToCanvasCoord config p0
      let pixeclCoordinatesP1 = cartesianToCanvasCoord config p1
      canvasLine <- Canvas.makePath2D
      canvasLine # Canvas.moveTo pixeclCoordinatesP0.canvasX pixeclCoordinatesP0.canvasY
      canvasLine # Canvas.lineTo pixeclCoordinatesP1.canvasX pixeclCoordinatesP1.canvasY
      canvasRef # Canvas.setStrokeStyle (colorToString color)
      canvasRef # Canvas.stroke canvasLine

-- TODO: Would be nice if CanvasRef actually contained width/height information
--       To get the width/height, you need to do an effect, since that may change.
render :: Canvas.CanvasConfig -> Canvas.CanvasRef -> Element -> Effect Unit
render config canvasRef el =
  el # traverse_ (renderPrimitiveElement config canvasRef)
