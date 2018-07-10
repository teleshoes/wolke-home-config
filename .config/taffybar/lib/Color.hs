module Color (
  Color(..), rgb, rgba, hexColor
) where

import Numeric (showHex)

data Color = Black
           | White
           | Gray
           | Red
           | Green
           | Blue
           | Yellow
           | Purple
           | Cyan
           | Orange
           | RGB (Double, Double, Double)
  deriving (Show)

rgb c = case c of
          Black  -> (0.0, 0.0, 0.0)
          White  -> (1.0, 1.0, 1.0)
          Gray   -> (0.5, 0.5, 0.5)
          Red    -> (1.0, 0.0, 0.0)
          Green  -> (0.0, 1.0, 0.0)
          Blue   -> (0.0, 0.0, 1.0)
          Yellow -> (1.0, 1.0, 0.0)
          Purple -> (1.0, 0.0, 1.0)
          Cyan   -> (0.0, 1.0, 1.0)
          Orange -> (1.0, 0.5, 0.0)
          RGB(r,g,b) -> (r,g,b)

rgba c a = (r,g,b,a)
  where (r,g,b) = rgb c

byte b = (if b < 16 then "0" else "") ++ showHex b ""

hexColor c = "#" ++ hex r ++ hex g ++ hex b
  where (r,g,b) = rgb c
        hex = byte . round . (255*)
