module PercentMonitor(main) where
import Data.List (intercalate, group, intersperse, zip4)
import Data.Maybe (fromMaybe, listToMaybe, isJust)
import Data.Char
import Control.Monad
import Control.Exception
import System.Environment.UTF8 (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, stdin, stdout, BufferMode(LineBuffering))
import Utils (height, fg, rect, posX, posAbsY, ignoreBG)

type Pix = Int
type Per = Float
type HeightPer = Per
type Height = Pix
type Width = Pix
type RelSample = [Per]
type AbsSample = [Height]
type Color = String

data SampleBlock = SampleBlock
    { sbWidth     :: Width
    , sbHeight    :: Height
    , sbColors    :: [Color]
    , sbAbsSample :: AbsSample
    }
packSampleBlock (h,w,c,p) = SampleBlock h w c p

data Rect = Rect
    { rWidth  :: Width
    , rHeight :: Height
    , rOffset :: Int
    , rColor  :: Color
    }
packRect (w,h,o,c) = Rect w h o c
packRects ws hs os cs = map packRect $ zip4 ws hs os cs

main = do
  colors <- fmap parseArgs getArgs
  let width = height
  drawMonitor (fromIntegral width) (fromIntegral height) colors stdin stdout

drawMonitor w height colors reader writer = do
  hSetBuffering writer LineBuffering
  let h = height-2
  let expectedLen = length colors
  let getRelSample = fmap (lineToRelSample expectedLen) (hGetLine reader)

  let loop (_:oldRelSamples) = do
      relSamples <- fmap ((oldRelSamples ++) . return) getRelSample
      hPutStrLn writer $ monitorMarkup w h colors relSamples
      loop relSamples
  loop (replicate w [])

parseArgs :: [String] -> [Color]
parseArgs (c1:c2:cs) = reverse $ c1:c2:cs
parseArgs _ = error "Usage: color1 color2 [color3 color4 ...]\n"


toPix = read :: String -> Pix
toPer = read :: String -> Per

isPix = maybe False id . fmap (null . snd) . listToMaybe . (reads :: ReadS Pix)
isPer = maybe False id . fmap (null . snd) . listToMaybe . (reads :: ReadS Per)

lineToRelSample :: Int -> String -> RelSample
lineToRelSample expectedLen line | ok = reverse $ map read pers
                                 | otherwise = error errorMsg
  where ok = all isPer pers && length pers == expectedLen
        pers = words line
        errorMsg = "Could not parse % line: " ++ line

monitorMarkup :: Width -> Height -> [Color] -> [RelSample] -> String
monitorMarkup w h colors relSamples = ignoreBG markup
  where markup = concatMap drawSampleBlock getSampleBlocks
        getAbsSamples = map (relToAbsSample h) relSamples
        getSampleBlocks = map sB $ group getAbsSamples
        sB :: [AbsSample] -> SampleBlock
        sB sPs = packSampleBlock (length sPs, h, colors, head sPs)

drawSampleBlock :: SampleBlock -> String
drawSampleBlock sb | isEmptySB sb = drawRect $ emptySampleBlockRect sb
                   | otherwise = sbMarkup
  where sbMarkup = concat $ intersperse reset $ map drawRect rects
        rects = filter ((>0).rHeight) $ sampleBlockToRects sb
        reset = posX (-sbWidth sb)

sampleBlockToRects :: SampleBlock -> [Rect]
sampleBlockToRects sb = packRects widths absSample offsets colors
  where widths = repeat $ sbWidth sb
        absSample = sbAbsSample sb
        offsets = scanl (+) 0 (sbAbsSample sb)
        colors = sbColors sb

isEmptySB :: SampleBlock -> Bool
isEmptySB sb = null (sbAbsSample sb)
emptySampleBlockRect :: SampleBlock -> Rect
emptySampleBlockRect sb = Rect (sbWidth sb) (sbHeight sb) 0 (head $ sbColors sb)

drawRect :: Rect -> String
drawRect r = fg (rColor r) $ posAbsY (rOffset r) ++ rect (rWidth r) (rHeight r)

relToAbsSample :: Height -> RelSample -> AbsSample
relToAbsSample h [] = []
relToAbsSample h relSample = ensureHeight h $ map (absPix h) relSample

absPix :: Height -> HeightPer -> Height
absPix totalHeight per = round $ (fromIntegral totalHeight) * (per/100.0)

ensureHeight :: Height -> AbsSample -> AbsSample
ensureHeight h px | sum px > h = ensureHeight h (decFirst h px)
                  | sum px < h = ensureHeight h (incFirst h px)
                  | otherwise = px
  where
        decFirst h = applyToFirstThat (+(0-1)) (>0)
        incFirst h = applyToFirstThat (+(0+1)) (<h)

applyToFirstThat :: (a -> a) -> (a -> Bool) -> [a] -> [a]
applyToFirstThat f on = (\(xs,(y:ys)) -> xs++(f y):ys) . break on
