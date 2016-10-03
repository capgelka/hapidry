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


import qualified Data.ByteString.Lazy.Char8 as BL -- (ByteString, pack, unpack)

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

m :: BL.ByteString
m = "{\"result\":\"12\",\"error\":\"\\u041d\\u0435\\u0432\\u0435\\u0440\\u043d\\u044b\\u0439 \\u0438\\u0434\\u0435\\u043d\\u0442\\u0438\\u0444\\u0438\\u043a\\u0430\\u0442\\u043e\\u0440 \\u0441\"}"


spec :: Spec
spec = do
  describe "keyHash" $ do
    it "creates md5 hash from password and secret key" $ do
      keyHash "123" "abf" `shouldBe` ("78d4ff855595972b916eac0d520496d1" :: Text)

  describe "ununicode" $ do
    it "doesn't change ascii text without unicode sequence" $ do
       (ununicode "just ascii: 1-9*&\\//\\0 \\urrwe\\ufeg eee") 
       >>= (\x -> x `shouldBe` "just ascii: 1-9*&\\//\\0 \\urrwe\\ufeg eee")
    it "converts url encoded string" $ do
        (ununicode "error:\\u041d\\u0435\\u0432\\u0435\\u0440\\u043d\\u044b\\u0439\\u100cc")
        >>= (\x ->  x `shouldBe` 
                      "error:\208\157\208\181\208\178\208\181\209\128\208\189\209\139\208\185\195\140")
    -- it "converts url encoded string" $ do
    --     (ununicode $ 
    --        BL.pack $ 
    --        concat $ 
    --        replicate 5000000 ("error:\\u041d\\u0435\\u0432\\u0435\\u0440\\u043d\\u044b\\u0439\\u100cc" :: String))
    --      >>= (\x -> (BL.last x) `shouldBe`
    --                   ('e'))
    -- it "converts url encoded string" $ do
    --     (ununicode m)
    --      `shouldBe`
    --      ("error:\208\157\208\181\208\178\208\181\209\128\208\189\209\139\208\185")

  describe "toCP1251" $ do
    it "converts letters to cp1251 encoding" $ do
      toCP1251 "Русский текст, и ascii"
      `shouldBe` 
      "\208\243\241\241\234\232\233 \242\229\234\241\242, \232 ascii"
    it "encodes unicode symbols as &#xxxx;" $ do
      toCP1251 "Русский текст, и アニメ"
      `shouldBe` 
      "\208\243\241\241\234\232\233 \242\229\234\241\242, \232 &#12450;&#12491;&#12513;"

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
      authRequest badClient  >>= (\x -> ( x & sid) `shouldBe` Left 67)
    it "returns Right constructor for sid in sid field for user with correct credentials" $ do
      authRequest goodClient >>= (\x -> ( x & sid) `shouldSatisfy` isRight)

  describe "apiPost" $ do 
    it "returns 67 error for request with wrong sid and login/password" $ do
      apiPost badClient [] `shouldReturn` Left 67
    it "returns 15 error for request with empty method" $ do
      apiPost goodClient [] `shouldReturn` Left 15
    it "returns response inside Right constructor for request with wrong sid but correct creds" $ do
      apiPost goodClient [("method","user.get")] >>= (`shouldSatisfy` isRight)
    it "returns response inside Right constructor for request with correct sid and incorrect creds" $ do
      client <- (\x -> badClient {sid = x & sid}) <$> authRequest goodClient
      apiPost client [("method","user.get")] >>= (`shouldSatisfy` isRight)

-- other tests needs functions to read data from the api for checks. So it'll be implemented later
