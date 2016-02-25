-- Examples of handling for JSON responses
--
-- This library provides several ways to handle JSON responses

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import Control.Lens ((&), (^.), (^?), (.~), set)
import Data.Aeson (FromJSON, fromJSON, decode, eitherDecode)
import Data.Aeson.Encode (encodeToTextBuilder, encodeToBuilder)
import Data.Aeson.Lens (key, _String, _Integer)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Either
import Data.Text (Text)
import qualified Data.Text.Lazy as L (toStrict)
import qualified Data.Text.Lazy.Encoding as LE (decodeLatin1, decodeUtf8)
import qualified Data.Text.Lazy.Builder as LB (toLazyText)
import Data.Text.Encoding (decodeLatin1, decodeUtf8)
import Data.Text.Read (decimal)
import Data.Binary (encode)
import qualified Data.ByteString.Base16 as B16 (encode)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (toLazyByteString)
import qualified Crypto.Hash.MD5 as MD5
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as BL --(ByteString)
import Numeric (showHex)
import qualified Data.Map as Map

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

data UmailData = UmailData {
    umailid :: Integer,
    from_username :: Text,
    dateline :: Text,
    read :: Text, 
    no_smilies :: Text,
    title :: Text,
    message_html :: Text
} deriving (Show, Generic)

data Umail = Umail {
    count :: Integer,
    umail :: UmailData
} deriving (Show, Generic)


instance FromJSON Umail
instance FromJSON UmailData

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
ununicode = map (\x -> let elem = Map.lookup x table in if elem /= Nothing then elem else Just x) where
  table = Map.fromList $ BL.zip letters rus
  --rus =  "GGG" --[1040 .. 1103] :: [Integer]
  rus = "ЁёАБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯабвгдежзийклмнопрстуфхцчшщъыьэюя" :: BL.ByteString
  --rus = "\1025\1105\1040\1041\1042\1043\1044\1045\1046\1047\1048\1049\1050\1051\1052\1053\1054\1055\1056\1057\1058\1059\1060\1061\1062\1063\1064\1065\1066\1067\1068\1069\1070\1071\1072\1073\1074\1075\1076\1077\1078\1079\1080\1081\1082\1083\1084\1085\1086\1087\1088\1089\1090\1091\1092\1093\1094\1095\1096\1097\1098\1099\1100\1101\1102\1103"
  letters = BL.concat $ map (\l -> BL.append "\\u" l)
                         ("0x0401":"0x0451":(map (\x -> encode (x :: Integer))
                                                      [0x0410 .. 0x044f]))

toOptions :: [(Text, Text)] -> Options
toOptions x = defaults & foldr (\(x, y) p -> p . set (param x) [y]) id x -- (\x f-> defaults x . f)

apiGet :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer BL.ByteString)
apiGet env p = apiGet' env (toOptions p) where
    apiGet' :: ClientCredentials -> Options -> IO (Either Integer BL.ByteString)
    apiGet' e params = case e & sid of
        (Left _)  -> return $ Left $ (-1)
        (Right x) -> apiGet'' $ params & param "sid" .~ [x] where -- & param "sid" .~ [x] where
            apiGet'' :: Options -> IO (Either Integer BL.ByteString)
            apiGet'' params = do
                -- print params
                r <- getWith params "http://www.diary.ru/api"
                -- print $ r ^? responseBody 
                -- print $ r ^? responseBody . key "result"
                -- print $ r ^? responseBody . key "result" . _String
                -- print $ r ^? responseBody . key "result" . _Integer
                -- print $ r ^? responseBody . key "result" . _String
                -- print $ r ^? responseBody . _String
                case r ^? responseBody . key "result" . _String of
                   (Just "0")  -> return $ Right $ r ^. responseBody
                   (Just "12") -> authRequest e >>= (\newEnv -> apiGet' newEnv params) 
                   (Just x)  -> return $ Left (-1) -- $ x
                   Nothing   -> return $ Left (-11)

userGet :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer BL.ByteString)
userGet env params = apiGet env (("method", "user.get"):params)


--umailGet :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer (Maybe Umail))
umailGet env params = do
    r <- apiGet env (("method", "umail.get"):params)
    x <- return $ case r of
          (Right a) -> a
          (Left a)  -> ""
    return $ LE.decodeUtf8 $ x
    -- return $ case r of
    --   (Right x) -> Right $ (decode x :: Maybe Umail )-- >>= (\u -> u & umail >>= return)
    --              -- do :: Maybe
    --              --  u <- decode x
    --              --  return $ u & umail 
    --   --(x & eitherDecode :: Either Umail) & "umail" -- Right $ decode x  -- :: Maybe Umail)
    --   (Left x) -> Left $ x

                     
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
             
  -- r <- authRequest client
  -- print r
  r2 <- umailGet client []
  -- print $ r2
  r3 <- apiGet client (("method", "umail.get"):[])
  -- let rr = (\(Right x) -> x >>= (\t -> Just $ t & umail  )) r2
  print $ r2
  print r3
  -- print $ r ^? responseBody . key "sid"
  -- -- (authorize appkey skey "hastest" "12341230") + 2
  -- a <- authorize appkey skey "hastest" "12341230"
  -- print $ a
  -- aa <- authorize appkey skey "hastest" "1234123"
  -- print $ aa
  -- print v
  -- print authRequest
  -- lens_aeson
