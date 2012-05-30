module PercentBar (percentBar) where
import System.Environment (getEnv)
import System.Process(readProcess)
import Utils (height)

percentBar :: Int -> [String] -> Int -> Int -> String
percentBar percent colors bgWidth fgWidth = markup
  where hs = height : (map (roundPercent height) $ pers percent)
        widths = bgWidth : repeat fgWidth
        markup = dzenRects height (max bgWidth fgWidth) hs widths colors

roundPercent max = round . (/100) . (* (fromInteger max)) . fromIntegral

pers p = replicate (p `div` 100) 100 ++ [p `mod` 100]

dzenRects maxH maxW hs ws cs =
  ""
  ++ "^p(" ++ show maxW ++ ";-8)"
  ++ "^ib(1)"
  ++ rects maxH hs ws cs
  ++ "^ib(0)"
  ++ "^p(;8)"

rects maxH (h:hs) (w:ws) (c:cs) = dzenRect maxH h w c ++ rects maxH hs ws cs
rects maxH _ _ _ = ""

dzenRect maxH h w c =
  ""
  ++ "^p(-" ++ show w ++ ";" ++ show startH ++ ")"
  ++ "^fg(" ++ c ++ ")"
  ++ "^r(" ++ show w ++ "x" ++ show h ++ ")"
  ++ "^fg()"
  ++ "^p(;-" ++ show startH ++ ")"
  where startH = maxH - h

