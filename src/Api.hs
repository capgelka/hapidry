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
  , umailsSend
  , notificationGet
  , commentCreate
  , commentsGet
  , postsGet
  , printError
  ) where


import qualified Data.ByteString.Lazy.Char8 as BL (ByteString, putStr, putStrLn)
import Data.Text (Text)
import qualified Data.Text.Lazy as L (Text)
import qualified Data.Text as T (head, tail)
import qualified Data.Text.IO as T
import Internal.Api (ClientCredentials(..), apiPost, authRequest)
--import Data.Aeson.Lens (key, _String)
import Data.Aeson
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Control.Lens ((&), (^.), (^?))
import qualified Json as J
import Control.Applicative
import Control.Monad
import Data.Maybe (catMaybes)

import System.Exit

type Name = Text
type Id = Text

postsCreate :: ClientCredentials -> [(Text, Text)] -> [Name] -> IO (Either BL.ByteString [BL.ByteString])
postsCreate env p [] = sequence . (: []) <$> postCreate env p
postsCreate env p names = do
  uids <- catMaybes <$> mapM (idByName env) names
  sequence <$> mapM (\u -> postCreate env (("juserid", u):p)) uids

postCreate :: ClientCredentials -> [(Text, Text)] -> IO (Either BL.ByteString BL.ByteString)
postCreate env p = apiPost env (("method", "post.create"):p)

userGet :: ClientCredentials -> [(Text, Text)] -> IO (Either BL.ByteString BL.ByteString)
userGet env params = apiPost env (("method", "user.get"):params)

umailGet :: ClientCredentials -> [(Text, Text)] -> IO (Either BL.ByteString BL.ByteString)
umailGet env params = apiPost env (("method", "umail.get"):params)

commentsGet :: ClientCredentials -> [(Text, Text)] -> Id -> IO (Either BL.ByteString BL.ByteString)
commentsGet env params pid | T.head pid == 'p' = commentsGet' env params (T.tail pid)
                           | otherwise = commentsGet' env params pid where
                                commentsGet' env params pid  =  apiPost env 
                                                                        (("method", "comment.get")
                                                                        :("postid", pid)
                                                                        :params)

postGet :: ClientCredentials -> [(Text, Text)] -> IO (Either BL.ByteString BL.ByteString)
postGet env params = apiPost env (("method", "post.get"):params)

postsGet :: ClientCredentials -> [(Text, Text)] -> [Name] -> IO (Either BL.ByteString [BL.ByteString])
postsGet env p [] = sequence . (: []) <$> postGet env (("type", "diary"):p)
postsGet env p names | "favorites" `elem` names = postsGet env (("type", "favorites"):p) []
                     | "last" `elem` names = postsGet env (("type", "last"):p) []
                     | "draft" `elem` names = postsGet env (("type", "draft"):p) []
                     | "quotes" `elem` names = postsGet env (("type", "quotes"):p) []
                     | otherwise = sequence <$> mapM (\j -> postGet env 
                                                                    (("type", "diary")
                                                                     :("shortname", j)
                                                                     :p)) 
                                                      names

umailsSend :: ClientCredentials -> [(Text, Text)] -> [Name] -> IO (Either BL.ByteString [BL.ByteString])
umailsSend env p [] = sequence <$> (: []) <$> umailSend env p
umailsSend env p names = sequence <$> mapM (\u -> umailSend env (("username", u):p)) names


umailSend :: ClientCredentials -> [(Text, Text)] -> IO (Either BL.ByteString BL.ByteString)
umailSend env params = apiPost env (("method", "umail.send"):("save_copy", "1"):params)

notificationGet :: ClientCredentials -> [(Text, Text)] -> IO (Either BL.ByteString BL.ByteString)
notificationGet env params = apiPost env (("method", "notification.get"):params)

-- | api call for creating comments
commentCreate :: ClientCredentials -> [(Text, Text)] -> IO (Either BL.ByteString BL.ByteString)
commentCreate env params = apiPost env (("method", "comment.create"):("subscribe", "1"):params)

maybeIdByName :: ClientCredentials -> Name -> IO (Maybe Id)
maybeIdByName env name = do
  response <- apiPost env [("method", "journal.get"), ("shortname", name)]
  return $ case response of
    (Right resp) -> J.userid <$> decode resp
    (Left _)     -> Nothing

idByName :: ClientCredentials -> Name -> IO (Maybe Id)
idByName env name = do
  response <- apiPost env [("method", "journal.get"), ("shortname", name)]
  case response of
    (Right resp) -> return $ J.userid <$> decode resp
    (Left resp)  -> printError resp >> return Nothing

printError :: BL.ByteString -> IO ()
printError json = case decode json of
   (Just r) -> do
      T.putStr "Ошибка: "
      T.putStrLn (r & J.errorText)
      exitWith $ ExitFailure (r & J.returnCode)
   Nothing  -> T.putStrLn "Unknown Error!" >> BL.putStr json >> exitWith (ExitFailure (-1))