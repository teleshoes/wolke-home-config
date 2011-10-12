#!/usr/bin/runghc
import Data.List (intercalate, group, intersperse)
import Data.Maybe (fromMaybe, listToMaybe, isJust)
import Data.Char
import Control.Monad
import Control.Applicative((<$>))
import Control.Exception
import System.Environment.UTF8 (getArgs)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))

main = do
  hSetBuffering stdout LineBuffering
  (w,h,colors) <- parseArgs <$> getArgs
  let loop i (_:oldSamples) = do
      samples <- ((oldSamples ++) . return) . reverse . getPercents (length colors) <$> getLine
      putStr $ formatSamples w h (reverse colors) samples
      putStr "\n"
      loop ((i+1) `mod` w) samples
  loop 0 $ replicate w []

i = read :: String -> Int
f = read :: String -> Float


isInt   = maybe False id . fmap (null . snd) . listToMaybe .
          (reads :: ReadS Int)
isFloat = maybe False id . fmap (null . snd) . listToMaybe .
          (reads :: ReadS Float)


parseArgs (w:h:c1:c2:cs) | isInt w && isInt h = (i w, i h, c1:c2:cs)
parseArgs _ = error "Usage: width height color1 color2 [color3 color4 ...]\n"

getPercents count line | ok = map f $ pers
                       | otherwise = error ("Could not parse % line: " ++ line)
  where pers = words line
        ok = all isFloat pers && length pers == count

formatSamples :: Int -> Int -> [String] -> [[Float]] -> String
formatSamples width height colors samples = "^ib(1)" ++ markup ++ "^ib(0)^fg()^pa()"
  where px = map (pixels (height-2)) samples
        markup = concatMap (\(px,w) -> drawRectStack w px colors) rectStacks
        rectStacks = map (\x->(head x, length x)) $ group px

drawRectStack width [] _ = "^p(" ++ (show width) ++ ")"
drawRectStack width heights colors = concat $ addResets $ map (rect width) hcos
  where hcos = filter ((>0).first) $ zip3 heights colors (offsets heights)
        offsets heights = scanl (+) 0 heights
        addResets = intersperse ("^p(-" ++ (show width) ++ ")")
        first (x,_,_) = x

rect width (height,color,offset) = ""
  ++ "^fg(" ++ color ++ ")"
  ++ "^pa(;" ++ (show offset) ++ ")"
  ++ "^r(" ++ (show width) ++ "x" ++ (show height) ++ ")"

pixels :: Int -> [Float] -> [Int]
pixels totalHeight [] = []
pixels totalHeight samplePers = ensureHeight totalHeight $ map px samplePers
  where px 0.0 = 0
        px per = if hpx per == 0 then 1 else hpx per
        hpx per = floor $ (fromIntegral totalHeight) * (per/100.0)
        ensureHeight :: Int -> [Int] -> [Int]
        ensureHeight h px | sum px > h = ensureHeight h (decFirst h px)
                          | sum px < h = ensureHeight h (incLast h px)
                          | otherwise = px
        applyToFirstThat f b = (\(xs,(y:ys)) -> xs++(f y):ys) . break b
        decFirst h = applyToFirstThat (+(0-1)) (>0)
        incFirst h = applyToFirstThat (+(0+1)) (<h)
        incLast h = reverse . (incFirst h) . reverse

