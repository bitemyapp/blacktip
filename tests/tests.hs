module Main where

import Control.Monad (replicateM)
import Database.Blacktip
import Data.List (sort)
import Test.Hspec

wirelessConfig :: Config
wirelessConfig = defaultConfig { interface = IName "wlan0" }

main :: IO ()
main = hspec $ do
  describe "timestamps generated are proper" $ do
    it "timestamps are well ordered" $ do
      results <- replicateM 100 (generateUniqueId wirelessConfig)
      sort results `shouldBe` results
