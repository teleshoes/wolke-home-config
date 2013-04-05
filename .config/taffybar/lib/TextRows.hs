module TextRows(textRows) where
import System.Environment.UTF8 (getArgs)
import Utils (height, lockX, posAbsY, shiftMid, ignoreBG, estimateLength)

textRows topText botText = topText ++ "\n" ++ botText
