{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Internal.Api
    ( ClientCredentials(..)
      , authRequest
      , keyHash
      , ununicode
      , toCP1251
      , toForm
      , apiPost
      , DiaryText(..)
    ) where


import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Text.Read (decimal)
import Data.Text.Lazy.Read (hexadecimal)
import qualified Data.Text.Lazy.Encoding as LE (decodeUtf8, encodeUtf8)
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL -- (ByteString, pack, unpack)
import qualified Data.ByteString.Base16 as B16 (encode)
import qualified Data.Text.Lazy as L (append, cons, tail, head, unpack, foldl, length, fromStrict,
                                      take, drop, pack, all, Text, snoc, last, singleton, empty)
import qualified Data.Text as T (unpack, map, concatMap, pack, snoc, filter, all)
import Data.Text (Text)
import Data.Aeson.Lens (key, _String)
import Control.Lens ((&), (^.), (^?))
import qualified Data.Map as Map
import qualified Network.Wreq.Types as NWTP (FormValue, renderFormValue)
import Network.Wreq

import qualified Data.Attoparsec.ByteString.Lazy as P
import qualified Data.Aeson.Parser as P
import Data.Aeson
import Control.Monad (mzero)

import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)
import Data.Text.Foreign (fromPtr)
import Data.Word (Word16)
import Foreign.Marshal.Array (lengthArray0 )

import Data.ByteString.Unsafe (unsafeUseAsCString)


foreign import ccall "udecode" udecode :: CString -> IO (Ptr Word16)

newtype DiaryText = DiaryText Text
instance NWTP.FormValue DiaryText where
    renderFormValue (DiaryText t) = toCP1251 t

-- all data needed for requests to api
data  ClientCredentials =  ClientCredentials {
      password :: B.ByteString,
      appkey   :: Text,
      sid      :: Either Integer Text,
      username :: Text,  
      secret   :: B.ByteString
      } deriving Show


keyHash :: B.ByteString -> B.ByteString -> Text
keyHash pass key = decodeLatin1 $ B16.encode $ MD5.hash $ B.append key pass 
   
{- to solve problem http://stackoverflow.com/questions/35687685/haskell-convert-unicode-sequence-to-utf-8
 no lib find -}

newtype MyBs = MyBs { unMyBs :: Text}  deriving (Eq, Show)

ununicode :: BL.ByteString -> IO (BL.ByteString)
ununicode x = do
  cs <- unsafeUseAsCString (BL.toStrict (BL.snoc x '\NUL')) udecode
  size <- fromIntegral <$> lengthArray0 0 cs
  LE.encodeUtf8 <$> L.fromStrict <$> fromPtr cs size


{- 
don't want dinamically linked icu, encoding lib doesn't compile 
http://stackoverflow.com/questions/35905276/cant-compile-haskell-encoding-lib-couldnt-find-haxml-modules
-}
-- | encodes Text as cp1251 encoded Bytestring. 
-- it also encodes non cyrrilic unicode symbols as &#xxxx; 
-- but is takes more time evem if only one suck sybbol exists in input
toCP1251 :: Text -> B.ByteString
toCP1251 = B.pack . T.unpack . unicodeText . T.map replace where

  replace :: Char -> Char
  replace l = case Map.lookup l table of
      (Just x) -> x
      Nothing  -> l

  isOneByte :: Text -> Bool
  isOneByte x = T.all (\e -> fromEnum e <= 255) x

  unicodeText :: Text -> Text
  unicodeText txt | isOneByte txt =  txt
                  | otherwise = T.concatMap unicode txt

  unicode :: Char -> Text
  unicode l | fromEnum l < 255 = T.pack [l]
            | otherwise = T.pack ('&':'#':(show $ fromEnum l)) `T.snoc` ';'


  table = Map.fromList $ zip rus cpCodes
  cpCodes = map toEnum (168:184:[192 .. 255]) :: String
  rus =  ['Ё', 'ё', 'А', 'Б', 'В', 'Г', 'Д', 'Е', 'Ж', 'З', 'И', 'Й', 'К', 'Л', 'М',
         'Н', 'О', 'П', 'Р', 'С', 'Т', 'У', 'Ф', 'Х', 'Ц', 'Ч', 'Ш', 'Щ', 'Ъ', 'Ы',
         'Ь', 'Э', 'Ю', 'Я', 'а', 'б', 'в', 'г', 'д', 'е', 'ж', 'з', 'и', 'й', 'к',
         'л', 'м', 'н', 'о', 'п', 'р', 'с', 'т', 'у', 'ф', 'х', 'ц', 'ч', 'ш', 'щ',
         'ъ', 'ы', 'ь', 'э', 'ю', 'я']  :: String

toForm :: [(Text, Text)] -> [FormParam]
toForm = map (\(x, y) -> encodeUtf8 x := DiaryText y)

apiPost :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer BL.ByteString)
apiPost e params = case e & sid of
    (Left x)  -> return $ Left x
    (Right x) ->  apiPost' $ updateSid x params where

        updateSid :: Text -> [(Text, Text)] -> [(Text, Text)]
        updateSid sid params = ("sid", sid):filter (\(x, _) -> x /= "sid") params

        apiPost' params = do
            r <- post "http://www.diary.ru/api" $ toForm params
            case r ^? responseBody . key "result" . _String of
               (Just "0")  -> return $ Right $ r ^. responseBody
               (Just "12") -> authRequest e >>= (`apiPost` params)
               (Just x)  -> return $ Left
                                   $ (\y -> case y of
                                          (Right a) -> fst a
                                          (Left _) -> (-1))
                                   $ decimal x 
               Nothing   -> return $ Left (-1)

authRequest :: ClientCredentials -> IO ClientCredentials
authRequest env = do
  r <- post "http://www.diary.ru/api" $ toForm [("appkey", appkey env),
                                                ("password", keyHash (password env)
                                                                     (secret env)),
                                                ("username", username env),
                                                ("method",  "user.auth")]
        
  return $ env { sid = authParse r } where 
    authParse :: Response BL.ByteString -> Either Integer Text
    authParse response = case response  ^? responseBody . key "result" of
            (Just "0") -> Right $ (response ^. responseBody . key "sid" . _String)
            (Just  _)  -> Left $ (\x -> case x of
                                      (Right a) -> fst a
                                      (Left _) -> 0)
                               $ decimal
                               $ response ^. responseBody . key "result" . _String
            Nothing    -> Left (-1)

convertTags :: [Text] -> [(Text, Text)]
convertTags = map (\t -> ("tags_data[]", t))