module CpuScalingCpufreq(cpuScalingCpufreqW, cpuGovW) where
import Utils (
  fg, bg, padL, regexGroups,
  readInt, collectInts, chompFile, readProc)
import Label (labelW, mainLabel)

import Control.Monad (void)
import Control.Monad.Trans.Either (EitherT(..), runEitherT)
import Control.Concurrent (forkIO)
import Control.Error (note)
import System.Process (system)
import Data.Functor ((<$>))
import Data.List (sort)
import Data.Maybe (fromMaybe, listToMaybe)

main = mainLabel cpuScalingCpuFreqReader
cpuScalingCpufreqW = labelW cpuScalingCpufreqReader

width = 2

cpuDir = "/sys/devices/system/cpu"

cpuScalingCpufreqReader = do
  cpu <- readCpu
  case cpu of
    Left err -> return err
    Right cur -> return $ formatScaling cur

cpuGovW = labelW $ do
  gov <- readGov
  case gov of
    Left err -> return err
    Right curGov -> return $ formatGov curGov

withErr act msg = EitherT $ note msg <$> act

allEq :: Eq a => [a] -> Bool
allEq xs = null xs || all (== head xs) (tail xs)

readCpu :: IO (Either String (String, Integer, Integer, [Integer]))
readCpu = runEitherT $ do
  gov <- getCpuField "scaling_governor"
         `withErr` "no governor!"
  minFreq <- getCpuFieldInt "scaling_min_freq"
             `withErr` "no min freq!"
  maxFreq <- getCpuFieldInt "scaling_max_freq"
             `withErr` "no max freq!"
  avail <- sort <$> getCpuFieldIntList "scaling_available_frequencies"
           `withErr` "no available frequencies!"
  return (gov, minFreq, maxFreq, avail)

readGov :: IO (Either String String)
readGov = runEitherT $ do
  gov <- getCpuField "governor"
         `withErr` "no governor!"
  return gov

getDevices :: String -> IO [String]
getDevices field = lines <$> readProc ["find", cpuDir, "-regex", regex]
  where regex = cpuDir ++ "/cpufreq/policy[0-9]+/" ++ field

getCpuField :: String -> IO (Maybe String)
getCpuField field = do
  devices <- getDevices field
  vals <- mapM chompFile devices
  return $ if allEq vals then listToMaybe vals else Nothing

getCpuFieldInt :: String -> IO (Maybe Integer)
getCpuFieldInt f = readInt <$> fromMaybe "" <$> getCpuField f

getCpuFieldIntList :: String -> IO (Maybe [Integer])
getCpuFieldIntList f = do
  ints <- collectInts <$> fromMaybe "" <$> getCpuField f
  return $ if null ints then Nothing else Just ints

formatScaling :: (String, Integer, Integer, [Integer]) -> String
formatScaling (gov, minKHz, maxKHz, avail) = color minKHz maxKHz avail text
  where pad = padL '0' width . take width
        fmt kHz = pad $ show $ kHz `div` 10^5
        text = fmt minKHz ++ "\n" ++ fmt maxKHz

formatGov :: String -> String
formatGov gov = color text
  where color = case gov of
                  "performance" -> bg "blue"
                  "powersave" -> bg "red" . fg "black"
                  "ondemand" -> bg "green"
                  "userspace" -> bg "yellow"
                  otherwise -> bg "orange" . fg "black"
        text = (take 2 gov) ++ "\n" ++ (take 2 $ drop 2 gov)

color :: Integer -> Integer -> [Integer] -> String -> String
color min max avail
  | min == low && max == high = bg "blue"
  | min == low && max == low = bg "red" . fg "black"
  | min == high && max == high = bg "green". fg "black"
  | otherwise = bg "orange" . fg "black"
  where low = if null avail then 0 else head avail
        high = if null avail then 0 else last avail
