module TextRows(textRows, main) where
import System.Environment.UTF8 (getArgs)
import Utils (height, lockX, posAbsY, shiftMid, ignoreBG, estimateLength)

topPx = 0 - (height `div` 12)
botPx = topPx + (height `div` 2)

main = do
 args <- getArgs
 putStr $ textRows (args !! 0) (args !! 1)

textRows topText botText = overlapMarkup shorter longer
  where (top, bot) = (posAbsY topPx ++ topText, posAbsY botPx ++ botText)
        (topLen, botLen) = (estimateLength top, estimateLength bot)
        (shorter, longer) = if topLen <= botLen then (top, bot) else (bot, top)

overlapMarkup bgM fgM = lockX bgM ++ ignoreBG fgM ++ shiftMid
