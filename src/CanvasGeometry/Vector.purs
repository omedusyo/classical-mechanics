module CanvasGeometry.Vector ( Vector, make, add, zero, scale, neg, sub ) where

import Prelude

type Vector = { dx :: Number, dy :: Number }

make :: Number -> Number -> Vector
make dx dy = { dx: dx, dy: dy }

add :: Vector -> Vector -> Vector
add v0 v1 =
  { dx: v0.dx + v1.dx, dy : v0.dy + v1.dy }

zero :: Vector
zero = { dx: 0.0, dy: 0.0 }

scale :: Number -> Vector -> Vector
scale k v =
  { dx: k*v.dx, dy: k*v.dy }

neg :: Vector -> Vector
neg v =
  { dx: -v.dx, dy: -v.dy }

sub :: Vector -> Vector -> Vector
sub v0 v1 = add v0 (neg v1)
