module CpuScaling(cpuScalingW, cpuGovW) where
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

main = mainLabel cpuScalingReader
cpuScalingW = labelW cpuScalingReader

width = 2
defaultGovernor = "performance"

tmpFile = "/tmp/cpu-scaling"
cpuDir = "/sys/devices/system/cpu"

cpuScalingReader = do
  cpu <- readCpu
  case cpu of
    Left err -> return err
    Right cur@(curGov, curMinFreq, curMaxFreq, curAvail) -> do
      let defMin = head curAvail
      let defMax = last curAvail
      tmp@(tmpGov, tmpMinFreq, tmpMaxFreq, tmpFreq) <-
        readTmp (defaultGovernor, defMin, defMax)
      let mismatchErr = getMismatchError cur tmp
      case mismatchErr of
        Just msg -> do
          cpuSet tmpGov tmpMinFreq tmpMaxFreq
          return $ "running cpu-set: " ++ msg
        Nothing -> return $ formatScaling cur

cpuGovW = labelW $ do
  gov <- readGov
  case gov of
    Left err -> return err
    Right curGov -> do
      tmp@(tmpGov, _, _, _) <- readTmp (defaultGovernor, 0, 0)
      if curGov /= tmpGov then do
          cpuSetGov tmpGov
          return $ "running cpu-set " ++ tmpGov
      else return $ formatGov curGov

withErr act msg = EitherT $ note msg <$> act

allEq :: Eq a => [a] -> Bool
allEq xs = null xs || all (== head xs) (tail xs)

readTmp :: (String, Integer, Integer) -> IO (String, Integer, Integer, Integer)
readTmp defaults = parseTmpFile defaults <$> chompFile tmpFile

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

cpuSetGov :: String -> IO ()
cpuSetGov gov = void $ forkIO $ void $ system cmd
  where cmd = "sudo cpu-set " ++ gov

parseTmpFile :: (String, Integer, Integer) -> String
             -> (String, Integer, Integer, Integer)
parseTmpFile (defGov, defMin, defMax) s = parseGroups $ fromMaybe defaults grps
  where re = ""
             ++ "governor=(.*)\\n?"
             ++ "min=(\\d*)\\n?"
             ++ "max=(\\d*)\\n?"
             ++ "freq=(\\d*)\\n?"
        grps = regexGroups re s
        defaults = [defGov, show defMin, show defMax, ""]
        parseGroups [g,min,max,freq] = (g, toInt min, toInt max, toInt freq)
        toInt = fromMaybe 0 . readInt

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
