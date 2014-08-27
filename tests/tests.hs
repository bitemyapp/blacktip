module Main where

import Control.Monad (replicateM)
import Database.Blacktip
import Data.List (sort)
import Test.Hspec

wirelessConfig :: Config
wirelessConfig = defaultConfig { interface = IName "wlan0" }

main :: IO ()
main = hspec $ do
  describe "identities generated are proper" $ do
    it "identities are well ordered" $ do
      results <- replicateM 100 (generateUniqueId wirelessConfig)
      sort results `shouldBe` results
    it "identities form an isomorphism" $ do
      result <- generateUniqueId' wirelessConfig
      fmap snd result `shouldBe` fmap (integerToRecord . fst) result
