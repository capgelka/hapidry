{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Json
    (
      Journal(..)
    , Notifications(..)
    ) where

-- import Data.Aeson.Lens (key, _String)
import Data.Aeson
import Data.Text (Text, pack)
import Control.Applicative
import Control.Monad
import qualified Data.HashMap.Strict as HMS
import Internal.Json

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



-- data CommentInfo = CommentInfo { cid :: Text, cprev :: Text }

-- instance FromJSON CommentInfo where
--   parseJSON (Object v) = CommentInfo <$> v .: "postid" <*> v .: "message_txt"
--   parseJSON _ = mzero


-- instance FromJSON Umail where
--   parseJSON (Object v) = Umail <$> v .: "from_username" 
--                                <*> v .: "title" 
--                                <*> v .: "message_txt"
--   parseJSON _ = mzero

-- instance FromJSON Discussion where
--   parseJSON (Object v) = Discussion <$> v .: "journal_name" 
--                                     <*> v .: "postid" 
--                                     <*> v .: "message_txt"
--   parseJSON _ = mzero

data Journal = Journal {
    userid    :: Text
  , shortname :: Text
  } deriving (Show, Eq)

data Notifications = Notifications {
    umailCount    :: Integer
  , discussCount  :: Integer
  , commentsCount :: Integer
  , discussions :: DiscussionList
  , umails :: UmailList
  , comments :: CommentList --[Comment]
  } deriving (Show, Eq)


-- newtype CommentList = CommentList [Comment] deriving (Show, Eq)

-- instance FromJSON CommentList where
--   parseJSON (Object v) = CommentList <$> HMS.foldrWithKey go (pure []) v
--     where
--       go i x r = (\(CommentInfo pid msg) rest -> Comment i msg : rest) <$>
--                      parseJSON x <*> r
--   parseJSON _ = empty

-- newtype UmailList = UmailList [Umail] deriving (Show, Eq)

-- instance FromJSON UmailList where
--   parseJSON (Object v) = UmailList <$> HMS.foldrWithKey go (pure []) v
--     where
--       go i x r = (\(Umail u t msg) rest -> Umail u t msg : rest) <$>
--                      parseJSON x <*> r
--   parseJSON _ = empty

-- newtype DiscussionList = DiscussionList [Discussion] deriving (Show, Eq)

-- instance FromJSON DiscussionList where
--   parseJSON (Object v) = DiscussionList <$> HMS.foldrWithKey go (pure []) v
--     where
--       go i x r = (\(Discussion j _ msg) rest -> Discussion j i msg : rest) <$>
--                      parseJSON x <*> r
--   parseJSON _ = empty

instance FromJSON Notifications where   
  parseJSON = withObject "notifications" $ \n -> do
    umailCount    <- (\x -> read x :: Integer) <$> n .: "umail_count"
    commentsCount <- (\x -> read x :: Integer) <$> n .: "comments_count"
    discussCount  <- (\x -> read x :: Integer) <$> n .: "discuss_count"
    (comments :: CommentList)  <- parseJSON =<< n .: "comments"
    (umails :: UmailList)  <- parseJSON =<< n .: "umail"  
    (discussions :: DiscussionList)  <- parseJSON =<< n .: "discuss" 
    return Notifications { umailCount = umailCount,
                           commentsCount = commentsCount,
                           discussCount = discussCount,
                           comments = comments,
                           umails = umails,
                           discussions = discussions }

instance FromJSON Journal where   
  parseJSON = withObject "journal" $ \j -> do
    journal   <- j .: "journal"
    userid    <- pack <$> journal .: "userid"
    shortname <- pack <$> journal .: "shortname"
    return Journal { userid = userid, shortname = shortname }