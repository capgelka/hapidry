{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec (Spec, describe, it, shouldSatisfy, shouldBe)
import Test.QuickCheck


main :: IO ()
main = hspec $ do
  describe "keyHash" $ do
    it "can parse integers" $ do
      read "10" `shouldBe` (10 :: Int)

    it "can parse floating-point numbers" $ do
      read "2.5" `shouldBe` (2.5 :: Float)

