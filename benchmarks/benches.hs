module Main where

import Control.Monad (replicateM)
import Control.Concurrent.Async (mapConcurrently)
import Criterion.Main
import qualified Database.Blacktip         as BT
import qualified Network.Info              as NI
import qualified Data.Time.Clock.POSIX     as PSX

import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE interf #-}
interf :: NI.NetworkInterface
interf =  fromJust $ unsafePerformIO $ BT.getInterfaceByName "wlan0"

wc :: BT.Config
wc = BT.defaultConfig { BT.interface = BT.NIInterface interf }

gen :: BT.Config -> IO (Either BT.NoInterfaceError BT.UniqueId)
gen = BT.generateUniqueId

genMany :: BT.Config -> Int -> IO [Either BT.NoInterfaceError BT.UniqueId]
genMany config n = mapConcurrently gen (replicate n config)

timeJustMultiply :: IO PSX.POSIXTime
timeJustMultiply = fmap (*1000) PSX.getPOSIXTime

main :: IO ()
main = defaultMain
  [ bench "one unique id"                 $ nfIO (gen wc)
  , bench "sequential 1000 unique ids"    $ nfIO (replicateM 1000 (gen wc))
  , bench "sequential 100,000 unique ids" $ nfIO (replicateM 100000 (gen wc))
  -- , bench "concurrent 1000 unique ids"    $ nfIO (genMany wc 1000)
  , bench "get Unix Millis"    $ nfIO BT.getUnixMillis
  -- , bench "get POSIX time"     $ nfIO PSX.getPOSIXTime
  -- , bench "POSIX time w/ mult" $ nfIO timeJustMultiply
  , bench "get interface by name" $ nfIO (BT.getInterfaceByName "wlan0")
  ]
