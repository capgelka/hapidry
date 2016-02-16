-- Examples of handling for JSON responses
--
-- This library provides several ways to handle JSON responses

{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import Control.Lens ((&), (^.), (^?), (.~))
import Data.Aeson (FromJSON)
import Data.Aeson.Lens (key)
import Data.Map (Map)
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import Data.Binary (encode)
import qualified Data.ByteString.Base16 as B16 (encode)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (toStrict)
import qualified Crypto.Hash.MD5 as MD5
    

import GHC.Generics (Generic)
import qualified Control.Exception as E

import Network.Wreq


-- This Haskell type corresponds to the structure of a response body
-- from httpbin.org.

data GetBody = GetBody {
    headers :: Map Text Text
  , args :: Map Text Text
  , origin :: Text
  , url :: Text
  } deriving (Show, Generic)


appkey = "6e5f8970828d967595661329239df3b5"
skey = "a503505ae803ee7f4fd477f01c1958b1"

-- digestToText :: MD5Digest -> Text
-- --digestToText = read .  (\s -> "\"" ++ s ++ "\"") . show
-- digestToText = decodeUtf8 . toStrict . encode

keyHash :: B.ByteString -> B.ByteString -> Text
-- keyHash pass key = read $ "\"" ++ show (md5 $ B.append key pass) ++ "\"" ::Text
keyHash pass key = decodeLatin1 $ B16.encode $ MD5.hash $ B.append key pass --need fix  to CP1251
                   -- in case hash of
                   --      (MD5Digest h) -> h
-- Get GHC to derive a FromJSON instance for us.

-- self.result = requests.get(
--     'http://www.diary.ru/api/?appkey={}&password={}&username={}&method=user.auth'.format(
--         self.appkey,
--         hashlib.md5(
--             self.skey + bytes(self.password, 'cp1251'))
--                 .hexdigest(),
--         self.username)).text
        -- self.sid = json.loads(self.result)['sid']
        -- print(type(self.sid))

authRequest appkey secret user password = getWith params "http://www.diary.ru/api" where
    params = defaults
             & param "appkey" .~ [appkey]
             & param "password" .~ [keyHash password secret]
             & param "username" .~ [user]
             & param "method" .~  ["user.auth"]

instance FromJSON GetBody



-- We expect this to succeed.

basic_asJSON :: IO ()
basic_asJSON = do
  let opts = defaults & param "foo" .~ ["bar"]
  r <- asJSON =<< getWith opts "http://httpbin.org/get"

  -- The fact that we want a GetBody here will be inferred by our use
  -- of the "headers" accessor function.

  putStrLn $ "args: " ++ show (args (r ^. responseBody))



-- The response we expect here is valid JSON, but cannot be converted
-- to an [Int], so this will throw a JSONError.

failing_asJSON :: IO ()
failing_asJSON = do
  (r :: Response [Int]) <- asJSON =<< get "http://httpbin.org/get"
  putStrLn $ "response: " ++ show (r ^. responseBody)



-- This demonstrates how to catch a JSONError.

failing_asJSON_catch :: IO ()
failing_asJSON_catch =
  failing_asJSON `E.catch` \(e :: JSONError) -> print e



-- Because asJSON is parameterized over MonadThrow, we can use it with
-- other instances.
--
-- Here, instead of throwing an exception in the IO monad, we instead
-- evaluate the result as an Either:
--
-- * if the conversion fails, the Left constructor will contain
--   whatever exception describes the error
--
-- * if the conversion succeeds, the Right constructor will contain
--   the converted response

either_asJSON :: IO ()
either_asJSON = do
  r <- get "http://httpbin.org/get"

  -- This first conversion attempt will fail, but because we're using
  -- Either, it will not throw an exception that kills execution.
  let failing = asJSON r :: Either E.SomeException (Response [Int])
  print failing

  -- Our second conversion attempt will succeed.
  let succeeding = asJSON r :: Either E.SomeException (Response GetBody)
  print succeeding



-- The lens package defines some handy combinators for use with the
-- aeson package, with which we can easily traverse parts of a JSON
-- response.

lens_aeson :: IO ()
lens_aeson = do
  r <- get "http://httpbin.org/get"
  print $ r ^? responseBody . key "headers" . key "User-Agent"

  -- If we maintain the ResponseBody as a ByteString, the lens
  -- combinators will have to convert the body to a Value every time
  -- we start a new traversal.

  -- When we need to poke at several parts of a response, it's more
  -- efficient to use asValue to perform the conversion to a Value
  -- once.

  let opts = defaults & param "baz" .~ ["quux"]
  v <- asValue =<< getWith opts "http://httpbin.org/get"
  print $ v ^? responseBody . key "args" . key "baz"



main :: IO ()
main = do
  -- basic_asJSON
  -- failing_asJSON_catch
  -- either_asJSON
  print skey
  print $ keyHash "1234123" skey 
  r <- authRequest appkey skey "hastest" "1234123"
  print $ r ^? responseBody
  lens_aeson
