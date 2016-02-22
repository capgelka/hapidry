-- Examples of handling for JSON responses
--
-- This library provides several ways to handle JSON responses

{-# LANGUAGE DeriveGeneric, OverloadedStrings#-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import Control.Lens ((&), (^.), (^?), (.~), set)
import Data.Aeson (FromJSON, fromJSON)
import Data.Aeson.Encode (encodeToTextBuilder, encodeToBuilder)
import Data.Aeson.Lens (key, _String, _Integer)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Either
import Data.Text (Text)
import qualified Data.Text.Lazy as L (toStrict)
import qualified Data.Text.Lazy.Encoding as LE (decodeLatin1)
import qualified Data.Text.Lazy.Builder as LB (toLazyText)
import Data.Text.Encoding (decodeLatin1)
import Data.Text.Read (decimal)
import Data.Binary (encode)
import qualified Data.ByteString.Base16 as B16 (encode)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (toLazyByteString)
import qualified Crypto.Hash.MD5 as MD5
import Control.Monad.Reader

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


--appkey = "6e5f8970828d967595661329239df3b5"
--skey = "a503505ae803ee7f4fd477f01c1958b1"

-- digestToText :: MD5Digest -> Text
-- --digestToText = read .  (\s -> "\"" ++ s ++ "\"") . show
-- digestToText = decodeUtf8 . toStrict . encode

-- all data needed for requests to api
data  ClientCredentials =  ClientCredentials {
      password :: B.ByteString,
      appkey  :: Text,
      sid     :: Either Integer Text,
      user    :: Text,  
      secret  :: B.ByteString
      } deriving Show

type Environment a = ReaderT ClientCredentials IO a

keyHash :: B.ByteString -> B.ByteString -> Text
-- keyHash pass key = read $ "\"" ++ show (md5 $ B.append key pass) ++ "\"" ::Text
keyHash pass key = decodeLatin1 $ B16.encode $ MD5.hash $ B.append key pass --need fix  to CP1251
                   -- in case hash of
                   --      (MD5Digest h) -> h
-- Get GHC to derive a FromJSON instance for us.

-- apiGet :: IO (Response B.ByteString)
-- sid :: Reader                  -- 

toOptions :: [(Text, Text)] -> Options
toOptions x = defaults & foldr (\(x, y) p -> p . set (param x) [y]) id x -- (\x f-> defaults x . f)

apiGet :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer Text)
apiGet env p = apiGet' env (toOptions p) where
    apiGet' :: ClientCredentials -> Options -> IO (Either Integer Text)
    apiGet' e params = case e & sid of
        (Left _)  -> return $ Left $ (-1)
        (Right x) -> apiGet'' $ params where -- & param "sid" .~ [x] where
            apiGet'' :: Options -> IO (Either Integer Text)
            apiGet'' params = do
                r <- getWith params "http://www.diary.ru/api"
                case r ^? responseBody . key "result" . _Integer of
                   (Just 0)  -> return $ Right $ r ^. responseBody . _String
                   (Just 12) ->  authRequest e >>= (\newEnv -> apiGet' newEnv params) 
                   (Just x)    -> return $ Left $ x
                   Nothing     -> return $ Left (-1)

                    -- (\x -> authRequest e >>= apiGet' x params) 


    -- r <- getWith params' "http://www.diary.ru/api"
    -- let code = r ^? responseBody . key "result"
    -- return $ case code of
    --            (Just "0")  -> r
    --            (Just "12") -> r
    --     where
    --       params' = params' & param "sid" .~ [sid]

         -- -           case response of
         --              (Right x) -> 

--                       where response = do
--                                     r <- getWith params "http://www.diary.ru/api" params

-- sid :: Reader B.ByteString B.ByteString
    
--authRequest :: Environment (IO ClientCredentials)--B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> IO (Response B.ByteString)
authRequest :: ClientCredentials -> IO ClientCredentials
authRequest env = do
  --r <- getWith params "http://www.diary.ru/api"
  -- let x = password env + 2
  let params = defaults
                            & param "appkey" .~ [appkey env]
                            & param "password" .~ [keyHash (password env) (secret env)]
                            & param "username" .~ [user env ]
                            & param "method" .~  ["user.auth"]
  r <- getWith params "http://www.diary.ru/api" -- >>= return 
  --let r =  
  -- r <- getWith params "http://www.diary.ru/api"                         
  return $ env { sid = (authParse r)} where --authParse $ getWith $ params
    -- authParse :: IO String -> Text
    -- authParse :: IO a -> IO b
    authParse response = case response  ^? responseBody . key "result" of
            (Just "0") -> Right  $ (response ^. responseBody . key "sid" . _String)
            (Just  x)  -> Left $ (\(Right a) -> fst a)
                               $ decimal
                               $ (response ^. responseBody . key "result" . _String)
            Nothing    -> Left $  (-1)



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

-- failing_asJSON :: IO ()
-- failing_asJSON = do
--   (r :: Response [Int]) <- asJSON =<< get "http://httpbin.org/get"
--   putStrLn $ "response: " ++ show (r ^. responseBody)



-- -- This demonstrates how to catch a JSONError.

-- failing_asJSON_catch :: IO ()
-- failing_asJSON_catch =
--   failing_asJSON `E.catch` \(e :: JSONError) -> print e



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

-- either_asJSON :: IO ()
-- either_asJSON = do
--   r <- get "http://httpbin.org/get"

--   -- This first conversion attempt will fail, but because we're using
--   -- Either, it will not throw an exception that kills execution.
--   let failing = asJSON r :: Either E.SomeException (Response [Int])
--   print failing

--   -- Our second conversion attempt will succeed.
--   let succeeding = asJSON r :: Either E.SomeException (Response GetBody)
--   print succeeding



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
  -- print skey
  -- print $ keyHash "1234123" skey
 -- print $ authRequest appkey skey "hastest" "1234123"
  -- let config = runReader ClientCredentials ClientCredentials {
  --               password = "1234123",
  --               appkey  = "6e5f8970828d967595661329239df3b5",
  --               sid     = "",
  --               user    = "hastest",  
  --               secret  = "a503505ae803ee7f4fd477f01c1958b1"
  --            }
             
  let client = ClientCredentials {
                password = "1234123",
                appkey  = "6e5f8970828d967595661329239df3b5",
                sid     = Right "",
                user    = "hastest",  
                secret  = "a503505ae803ee7f4fd477f01c1958b1"
             }
             
  r <- authRequest client
  print r
  -- print $ r ^? responseBody . key "sid"
  -- -- (authorize appkey skey "hastest" "12341230") + 2
  -- a <- authorize appkey skey "hastest" "12341230"
  -- print $ a
  -- aa <- authorize appkey skey "hastest" "1234123"
  -- print $ aa
  -- print v
  -- print authRequest
  -- lens_aeson
