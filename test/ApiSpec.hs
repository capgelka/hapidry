{-# LANGUAGE  OverloadedStrings #-}

module ApiSpec (spec) where

import Test.Hspec -- (Spec, describe, it, shouldSatisfy, shouldBe)
import Test.QuickCheck
import Internal.Api
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)
import qualified Data.ByteString.Char8 as B (ByteString)
import Network.Wreq
import Network.Wreq.Types (FormValue, renderFormValue)
import Control.Lens ((&), (^.), (^?))
import Data.Either (isRight)

-- main :: IO ()
-- main = hspec spec
instance Eq FormParam where
  (==) (x := y) (a := b)  =  x == a && (renderFormValue y == renderFormValue b)


badClient = ClientCredentials {
                password = "no",
                appkey  = "5ab793910e36584cd81622e5eb77d3d1",
                sid     = Right "",
                username    = "username",  
                secret  = "8543db8deccb4b0fcb753291c53f8f4f"
              } 

goodClient = ClientCredentials {
                password = "1234123",
                appkey  = "5ab793910e36584cd81622e5eb77d3d1",
                sid     = Right "",
                username    = "чутест",  
                secret  = "8543db8deccb4b0fcb753291c53f8f4f"
              }

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
  describe "toCP1251" $ do
    it "converts letters to cp1251 encoding" $ do
      toCP1251 "Русский текст, и ascii"
      `shouldBe` 
      "\208\243\241\241\234\232\233 \242\229\234\241\242, \232 ascii"
  describe "toForm" $ do
    it "converts list of pairs for post request to wreq Internal representation" $ do
      (toForm [("param","value"), ("param2", "кириллица")] :: [FormParam])
      `shouldBe`
      ([
        ("param" :: B.ByteString) := (DiaryText "value"),
        ("param2" :: B.ByteString) := (DiaryText "кириллица")
        ] :: [FormParam])
  describe "authRequest" $ do
    it "returns 67 error in sid field for user with bad credentials" $ do
      authRequest badClient >>= (\x ->  ( x & sid) `shouldBe` Left 67)
    it "returns Right constructor for sid in sid field for user with correct credentials" $ do
      authRequest goodClient >>= (\x ->  ( x & sid) `shouldSatisfy` isRight)

