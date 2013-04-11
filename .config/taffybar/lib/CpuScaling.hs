module CpuScaling(cpuScalingW) where
import Utils (
  fg, bg, padL, regexGroups,
  readInt, collectInts, chompFile, readProc)
import Widgets (label)

import Control.Monad (void)
import Control.Concurrent (forkIO)
import Control.Error (EitherT(..), runEitherT, note)
import System.Process (system)
import Data.Functor ((<$>))
import Data.List (sort)
import Data.Maybe (fromMaybe, listToMaybe)

width = 2

tmpFile = "/tmp/cpu-scaling"
cpuDir = "/sys/devices/system/cpu"

cpuScalingW = label $ do
  cpu <- readCpu
  case cpu of
    Left err -> return err
    Right cur@(curGov, curMinFreq, curMaxFreq, curAvail) -> do
      tmp@(tmpGov, tmpMinFreq, tmpMaxFreq, tmpFreq) <- readTmp curAvail
      let mismatchErr = getMismatchError cur tmp
      case mismatchErr of
        Just msg -> do
          cpuSet tmpGov tmpMinFreq tmpMaxFreq
          return $ "running cpu-set: " ++ msg
        Nothing -> return $ formatScaling cur

withErr act msg = EitherT $ note msg <$> act

allEq :: Eq a => [a] -> Bool
allEq xs = null xs || all (== head xs) (tail xs)

readTmp :: [Integer] -> IO (String, Integer, Integer, Integer)
readTmp avail = parseTmpFile avail <$> chompFile tmpFile

readCpu :: IO (Either String (String, Integer, Integer, [Integer]))
readCpu = runEitherT $ do
  gov <- getCpuField "governor"
         `withErr` "no governor!"
  minFreq <- getCpuFieldInt "min_freq"
             `withErr` "no min freq!"
  maxFreq <- getCpuFieldInt "max_freq"
             `withErr` "no max freq!"
  avail <- sort <$> getCpuFieldInts "available_frequencies"
           `withErr` "no available frequencies!"
  return (gov, minFreq, maxFreq, avail)

getDevices :: String -> IO [String]
getDevices field = lines <$> readProc ["find", cpuDir, "-regex", regex]
  where regex = cpuDir ++ "/cpu[0-9]+/cpufreq/scaling_" ++ field

getCpuField :: String -> IO (Maybe String)
getCpuField field = do
  devices <- getDevices field
  vals <- mapM chompFile devices
  return $ if allEq vals then listToMaybe vals else Nothing

getCpuFieldInt :: String -> IO (Maybe Integer)
getCpuFieldInt f = readInt <$> fromMaybe "" <$> getCpuField f

getCpuFieldInts :: String -> IO (Maybe [Integer])
getCpuFieldInts f = do
  ints <- collectInts <$> fromMaybe "" <$> getCpuField f
  return $ if null ints then Nothing else Just ints

getMismatchError :: (String, Integer, Integer, [Integer])
               -> (String, Integer, Integer, Integer)
               -> Maybe String
getMismatchError cur tmp
  | gov /= tmpGov = Just "mismatched governor!"
  | minFreq /= tmpMinFreq = Just "mismatched min freq!"
  | maxFreq /= tmpMaxFreq = Just "mismatched max freq!"
  | 0 /= tmpFreq = Just "mismatched exact freq!"
  | otherwise = Nothing
  where (gov, minFreq, maxFreq, avail) = cur
        (tmpGov, tmpMinFreq, tmpMaxFreq, tmpFreq) = tmp

cpuSet :: String -> Integer -> Integer -> IO ()
cpuSet gov minFreq maxFreq = void $ forkIO $ void $ system cmd
  where cmd = "sudo cpu-set "
              ++ gov ++ " " ++ show minFreq ++ " " ++ show maxFreq

parseTmpFile :: [Integer] -> String -> (String, Integer, Integer, Integer)
parseTmpFile avail s = parseGroups $ fromMaybe defaultTmp grps
  where re = ""
             ++ "governor=(.*)\\n?"
             ++ "min=(\\d*)\\n?"
             ++ "max=(\\d*)\\n?"
             ++ "freq=(\\d*)\\n?"
        grps = regexGroups re s
        defaultTmp = ["ondemand", show $ head avail, show $ last avail, ""]
        parseGroups [g,min,max,freq] = (g, toInt min, toInt max, toInt freq)
        toInt = fromMaybe 0 . readInt

formatScaling :: (String, Integer, Integer, [Integer]) -> String
formatScaling (gov, minKHz, maxKHz, avail) = color minKHz maxKHz avail text
  where pad = padL '0' width . take width
        fmt kHz = pad $ show $ kHz `div` 10^5
        text = fmt minKHz ++ "\n" ++ fmt maxKHz

color :: Integer -> Integer -> [Integer] -> String -> String
color min max avail
  | min == low && max == high = bg "blue"
  | min == low && max == low = bg "red" . fg "black"
  | min == high && max == high = bg "green". fg "black"
  | otherwise = bg "orange" . fg "black"
  where low = if null avail then 0 else head avail
        high = if null avail then 0 else last avail
