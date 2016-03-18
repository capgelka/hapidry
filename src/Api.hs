{-# LANGUAGE  OverloadedStrings #-}

module Api
  ( ClientCredentials(..)
  , postCreate
  , userGet
  , umailGet
  , umailSend
  , authRequest
  ) where


import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)
import Data.Text (Text)
import qualified Data.Text.Lazy as L (Text)
import Internal.Api (ClientCredentials(..), apiPost, authRequest)
import Data.Aeson.Lens (key, _String)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)


type Name = Text

postCreate :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer BL.ByteString)
postCreate env p = apiPost env (("method", "post.create"):p)

userGet :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer BL.ByteString)
userGet env params = apiPost env (("method", "user.get"):params)


umailGet :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer BL.ByteString)
umailGet env params = apiPost env (("method", "umail.get"):params)


umailSend :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer BL.ByteString)
umailSend env params = apiPost env (("method", "umail.send"):("save_copy", "1"):params)


nameById :: ClientCredentials -> Name -> IO (Maybe L.Text)
nameById env name = do
  response <- apiPost env [("method", "journal.get"), ("shortname", name)]
  -- print <$> response . key "shortname" . _String
  return $ case response of
    (Right resp) -> Just $ decodeUtf8 $ resp -- . key "userid" -- . _String)
    (Left _)     -> Nothing
