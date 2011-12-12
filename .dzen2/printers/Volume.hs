module Volume (main) where
import System.Environment (getEnv)
import System.Process(readProcess)

height = 36

mutedColors = ["yellow", "red", "pink"] ++ repeat "orange"
unmutedColors = ["black", "green", "blue"] ++ repeat "orange"

main = do
  home <- getEnv "HOME"
  vol <- fmap i $ readProcess (home ++ "/bin/pulse-volume") [] ""
  mute <- fmap isMuted $ readProcess (home ++ "/bin/pulse-mute") ["sink"] ""

  let bgWidth = 20
  let fgWidth = 5

  let heights = height : (map (roundPercent height) $ pers $ vol)
  let widths = bgWidth : repeat fgWidth
  let colors = if mute then mutedColors else unmutedColors
  putStr $ formatBars height (max bgWidth fgWidth) heights widths colors

roundPercent max = round . (/100) . (* (fromInteger max)) . fromIntegral

pers p = replicate (p `div` 100) 100 ++ [p `mod` 100]

formatBars maxH maxW hs ws cs =
  ""
  ++ "^p(" ++ show maxW ++ ";-8)"
  ++ "^ib(1)"
  ++ bars maxH hs ws cs
  ++ "^ib(0)"
  ++ "^p(;8)"

bars maxH (h:hs) (w:ws) (c:cs) = rect maxH h w c ++ bars maxH hs ws cs
bars maxH _ _ _ = ""

rect maxH h w c = ""
                  ++ "^p(-" ++ show w ++ ";" ++ show startH ++ ")"
                  ++ "^fg(" ++ c ++ ")"
                  ++ "^r(" ++ show w ++ "x" ++ show h ++ ")"
                  ++ "^fg()"
                  ++ "^p(;-" ++ show startH ++ ")"
                  where startH = maxH - h

i = read :: String -> Int

isMuted sink | (lines sink !! 0) == "sink is muted" = True
             | (lines sink !! 0) == "sink is not muted" = False
             | otherwise = error ("unknown speaker status: " ++ sink)

