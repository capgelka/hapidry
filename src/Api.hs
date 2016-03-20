{-# LANGUAGE  OverloadedStrings #-}

module Api
  ( ClientCredentials(..)
  , postCreate
  , userGet
  , umailGet
  , umailSend
  , authRequest
  , idByName
  , postsCreate
  ) where


import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)
import Data.Text (Text)
import qualified Data.Text.Lazy as L (Text)
import Internal.Api (ClientCredentials(..), apiPost, authRequest)
import Data.Aeson.Lens (key, _String)
import Data.Aeson
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Control.Lens ((&), (^.), (^?))
import Json
import Control.Applicative
import Control.Monad
import Data.Maybe (catMaybes)


type Name = Text
type Id = Text

postsCreate :: ClientCredentials -> [(Text, Text)] -> [Name] -> IO (Either Integer [BL.ByteString])
postsCreate env p [] = sequence <$> liftM (\x -> [x]) (postCreate env p)
postsCreate env p names = do
  uids <- catMaybes <$> mapM (idByName env) names
  --let u = uids + 1 -- sequence ids
  -- helper $ Right "" where
  --     helper :: [Either Integer BL.ByteString] -> Either Integer [BL.ByteString]
  --     he
  sequence <$> mapM (\u -> postCreate env (("juserid", u):p)) uids
  --(\x -> x:[]) <$> postCreate env (p ++ [("juserid", fromJust (uids !! 1))])
  -- mapM (\u -> do 
  --         res <- case (postCreate env [("juserid", u)]) of
  --           (Right b)  -> b
  --           (Left err) -> ""
  --         res
  --       ) (sequence uids)
  -- map (\n -> postCreate env [("juserid", uid)]

  -- apiPost env (("method", "post.create"):p)

postCreate :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer BL.ByteString)
postCreate env p = apiPost env (("method", "post.create"):p)

userGet :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer BL.ByteString)
userGet env params = apiPost env (("method", "user.get"):params)


umailGet :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer BL.ByteString)
umailGet env params = apiPost env (("method", "umail.get"):params)


umailSend :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer BL.ByteString)
umailSend env params = apiPost env (("method", "umail.send"):("save_copy", "1"):params)


idByName :: ClientCredentials -> Name -> IO (Maybe Id)
idByName env name = do
  response <- apiPost env [("method", "journal.get"), ("shortname", name)]
  return $ case response of
    (Right resp) -> userid <$> decode resp
    (Left _)     -> Nothing
