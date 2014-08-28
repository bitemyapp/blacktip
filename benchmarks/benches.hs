module Main where

import Control.Monad (replicateM)
import Control.Concurrent.Async (mapConcurrently)
import Criterion.Main
import qualified Database.Blacktip as BT

wc :: BT.Config
wc = BT.defaultConfig { BT.interface = BT.IName "wlan0" }

gen :: BT.Config -> IO (Either BT.NoInterfaceError BT.UniqueId)
gen = BT.generateUniqueId

genMany :: BT.Config -> Int -> IO [Either BT.NoInterfaceError BT.UniqueId]
genMany config n = do
  results <- mapConcurrently gen (replicate n config)
  return results

main :: IO ()
main = defaultMain
  [ bench "one unique id"  $ nfIO (gen wc)
  , bench "sequential 100 unique ids" $ nfIO (replicateM 100 (gen wc))
  , bench "concurrent 100 unique ids" $ nfIO (genMany wc 100)
  ]
