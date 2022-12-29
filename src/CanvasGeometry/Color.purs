module CanvasGeometry.Color where

import Prelude

data Color = RGBA Int Int Int Number

instance showColor :: Show Color where
  show (RGBA r g b alpha) = "rgba(" <> show r <> ", " <> show g <> ", " <> show b <> "; " <> show alpha <> ")"

rgba :: Int -> Int -> Int -> Number -> Color
rgba = RGBA

rgb :: Int -> Int -> Int -> Color
rgb r g b = RGBA r g b 1.0

black :: Color
black = rgb 0 0 0

white :: Color
white = rgb 255 255 255 

red :: Color
red = rgb 255 0 0

green :: Color
green = rgb 0 255 0

blue :: Color
blue = rgb 0 0 255
