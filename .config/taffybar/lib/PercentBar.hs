module PercentBar (percentBar) where
import System.Environment (getEnv)
import System.Process(readProcess)
import Utils (height, fg, rect, pos, posY, ignoreBG)
import Control.Arrow (first, second)

percentBar percent colors width = pos width (-8) ++ ignoreBG rects ++ posY 8
  where hs = heights percent
        ws = iterate ((max 1) . (`divr` 5) . (*3)) width
        rects = concatMap drawRect $ zip3 hs ws (cycle colors)

divr x y = round $ (fromIntegral x) / (fromIntegral y)

drawRect (h,w,c) = pos (-w) startH ++ fg c (rect w h) ++ posY (-startH)
  where startH = height - (fromIntegral h)

heights p = replicate fullBars height ++ [(height * partialBar) `divr` 100]
  where (fullBars,partialBar) = second fromIntegral $ first (+1) $ p`divMod`100

