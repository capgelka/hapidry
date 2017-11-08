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
import Data.Aeson
import Json


import qualified Data.ByteString.Lazy.Char8 as BL -- (ByteString, pack, unpack)

extractError :: Either BL.ByteString a -> Text
extractError (Left json) = case decode json of
   (Just r) -> r & errorText
   Nothing  -> error "Unhandled Exception! Impossible Case"
extractError _ = error "Unhandled Exception! Impossible Case"

instance Eq FormParam where
  (==) (x := y) (a := b)  =  x == a && (renderFormValue y == renderFormValue b)


badClient = ClientCredentials {
                password = "no",
                appkey  = "5ab793910e36584cd81622e5eb77d3d1",
                sid     = Right "",
                username    = "username",  
                secret  = "8543db8deccb4b0fcb753291c53f8f4f",
                endpoint = "https://secure.diary.ru/api"
              } 

goodClient = ClientCredentials {
                password = "1234123",
                appkey  = "5ab793910e36584cd81622e5eb77d3d1",
                sid     = Right "",
                username    = "чутест",  
                secret  = "8543db8deccb4b0fcb753291c53f8f4f",
                endpoint = "https://secure.diary.ru/api"
              }

m :: BL.ByteString
m = "{\"result\":\"12\",\"error\":\"\\u041d\\u0435\\u0432\\u0435\\u0440\\u043d\\u044b\\u0439 \\u0438\\u0434\\u0435\\u043d\\u0442\\u0438\\u0444\\u0438\\u043a\\u0430\\u0442\\u043e\\u0440 \\u0441\"}"


spec :: Spec
spec = do
  describe "keyHash" $ do
    it "creates md5 hash from password and secret key" $ do
      keyHash "123" "abf" `shouldBe` ("78d4ff855595972b916eac0d520496d1" :: Text)

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
    it "returns wrong credentials error in sid field for user with bad credentials" $ do
      authRequest badClient  >>= (\x -> extractError ( x & sid) `shouldBe` "Неверный логин или пароль")
    it "returns Right constructor for sid in sid field for user with correct credentials" $ do
      authRequest goodClient >>= (\x -> ( x & sid) `shouldSatisfy` isRight)
                                                                                                                                                                                                                        

  describe "apiPost" $ do 
    it "returns wrong credential error error for request with wrong sid and login/password" $ do
      (apiPost badClient [] >>= \x -> return $ extractError x) `shouldReturn` "Неверный логин или пароль"
    it "returns unspecified method error for request with empty method" $ do
      (apiPost goodClient [] >>= \x -> return $ extractError x) `shouldReturn` "Метод не определен"
    it "returns response inside Right constructor for request with wrong sid but correct creds" $ do
      apiPost goodClient [("method","user.get")] >>= (`shouldSatisfy` isRight)
    it "returns response inside Right constructor for request with correct sid and incorrect creds" $ do
      client <- (\x -> badClient {sid = x & sid}) <$> authRequest goodClient
      apiPost client [("method","user.get")] >>= (`shouldSatisfy` isRight)

