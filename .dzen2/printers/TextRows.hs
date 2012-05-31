module TextRows(textRows, main) where
import System.Environment.UTF8 (getArgs)
import Utils (height, posAbsY, estimateLength)

topPx = 0 - (height `div` 12)
botPx = topPx + (height `div` 2)

main = do
 args <- getArgs
 putStr $ textRows (args !! 0) (args !! 1)

textRows topText botText = overlapMarkup shorter longer
  where (top, bot) = (posAbsY topPx topText, posAbsY botPx botText)
        (topLen, botLen) = (estimateLength top, estimateLength bot)
        (shorter, longer) = if topLen <= botLen then (top, bot) else (bot, top)

overlapMarkup bg fg = ""
  ++ "^p(_LOCK_X)"
  ++ bg
  ++ "^p(_UNLOCK_X)"
  ++ "^ib(1)"
  ++ fg
  ++ "^ib(0)"
  ++ "^pa()"
