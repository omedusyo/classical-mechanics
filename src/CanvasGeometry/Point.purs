module CanvasGeometry.Point ( Point, make, origin, add, sub ) where

import Prelude
import CanvasGeometry.Vector as Vector
type Vector = Vector.Vector


type Point = { x :: Number, y :: Number }

make :: Number -> Number -> Point
make x y = { x: x, y: y }

origin :: Point
origin = { x: 0.0, y: 0.0 }

add :: Point -> Vector -> Point
add p v =
  { x: p.x + v.dx, y: p.y + v.dy }

sub :: Point -> Point -> Vector
sub p0 p1 =
  { dx: p0.x - p1.x, dy: p0.y - p1.y }
