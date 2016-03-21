{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Json
    (
      Journal(..)
    ) where

import Data.Aeson.Lens (key, _String)
import Data.Aeson
import Data.Text (Text, pack)
import GHC.Generics
import Control.Applicative

-- discuss_count - общее число непрочитанных комментариев в дискуссиях;
-- discuss - дискусии:
-- discuss.juserid - идентификатор дневника;
-- discuss.postid - идентификатор записи;
-- discuss.dateline - дата последнего комментария;
-- discuss.count - количество непрочитанных комментариев в записи;
-- discuss.journal_name - название дневника/сообщества;
-- comments_count - общее число непрочитанных комментариев в дневнике;
-- comments - комментарии:
-- comments.postid - идентификатор записи;
-- comments.message_txt - начало (первые 100 символов) текста комментария;
-- umail_count - общее число непрочитанных писем u-mail;
-- umail - непрочитанные письма:
-- umail.umailid - идентификатор письма;
-- umail.from_userid - идентификатор отправителя;
-- umail.from_username - логин отправителя;
-- umail.dateline - дата-время отправления;
-- umail.message_txt - начало (первые 100 символов) текста письма;
-- umail.title - заголовок письма;
-- umail.folder - идентификатор папки.

type Preview = Text
type Id = Text
type JournalName = Text
type Title = Text
type Username = Text
data Discussion = Discussion JournalName Id Preview deriving (Show, Eq)
data Umail = Umail Username Title Preview deriving (Show, Eq)
data Comment = Comment Id Preview deriving (Show, Eq)

data Journal = Journal {
    userid    :: Text
  , shortname :: Text
  } deriving (Show, Generic, Eq)

data Notifications = Notifications {
    umailCount    :: Integer
  , discussCount  :: Integer
  , commentsCount :: Integer
  , discussions :: [Discussion]
  , umails :: [Umail]
  , comments :: [Comment]
  } deriving (Show, Generic, Eq)


instance FromJSON Notifications where   
  parseJSON = withObject "notifications" $ \n -> do
    umailCount    <- n .: "umail_count"
    commentsCount <- n .: "comments_count"
    discussCount  <- n .: "discuss_count"
    c             <- parseArray "comments"
    -- comments      <- map (\x ->)

    -- userid <- pack <$> journal .: "userid"
    -- shortname <- pack <$> journal .: "shortname"
    return Notifications { umailCount = umailCount,
                           commentsCount = commentsCount,
                           discussCount = discussCount }

instance FromJSON Journal where   
  parseJSON = withObject "journal" $ \j -> do
    journal   <- j .: "journal"
    userid    <- pack <$> journal .: "userid"
    shortname <- pack <$> journal .: "shortname"
    return Journal { userid = userid, shortname = shortname }