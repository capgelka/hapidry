{-# LANGUAGE  OverloadedStrings #-}

module ApiSpec (spec) where

import Test.Hspec -- (Spec, describe, it, shouldSatisfy, shouldBe)
import Test.QuickCheck
import Internal.Api
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)


-- main :: IO ()
-- main = hspec spec

spec :: Spec
spec = do
  describe "keyHash" $ do
    it "creates md5 hash from password and secret key" $ do
      keyHash "123" "abf" `shouldBe` ("78d4ff855595972b916eac0d520496d1" :: Text)

  describe "ununicode" $ do
    it "doesn't change ascii text without unicode seauence" $ do
       (ununicode "just ascii: 1-9*&\\//\0 \\urrwe\\ufeg eee")
        `shouldBe`
        "just ascii: 1-9*&\\//\0 \\urrwe\\ufeg eee"
    it "converts url encoded string" $ do
        (ununicode "error:\\u041d\\u0435\\u0432\\u0435\\u0440\\u043d\\u044b\\u0439")
         `shouldBe`
         ("error:\208\157\208\181\208\178\208\181\209\128\208\189\209\139\208\185")
