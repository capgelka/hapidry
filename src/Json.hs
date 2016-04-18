{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Json
    (
      Journal(..)
    , Notifications(..)
    , J.PostList(..)
    , UmailList(..)
    , Umail(..) --temp
    ) where

import Data.Aeson
import Data.Text (Text, pack)
import Control.Applicative
import Control.Monad
import qualified Data.HashMap.Strict as HMS
import qualified Internal.Json as J


data Journal = Journal {
    userid    :: Text
  , shortname :: Text
  } deriving (Show, Eq)

data Notifications = Notifications {
    umailCount    :: Integer
  , discussCount  :: Integer
  , commentsCount :: Integer
  , discussions :: J.DiscussionList
  , umails :: J.UmailList
  , comments :: J.CommentList 
  } deriving (Show, Eq)

-- newtype PostList = PostList [Post] deriving (Eq, Show)



-- instance FromJSON Notifications where   
--   parseJSON = withObject "notifications" $ \n -> do
--     umailCount    <- (\x -> read x :: Integer) <$> n .: "umail_count"
--     commentsCount <- (\x -> read x :: Integer) <$> n .: "comments_count"
--     discussCount  <- (\x -> read x :: Integer) <$> n .: "discuss_count"
--     (comments :: CommentList)  <- parseJSON =<< n .: "comments"
--     (umails :: UmailList)  <- parseJSON =<< n .: "umail"  
--     (discussions :: DiscussionList)  <- parseJSON =<< n .: "discuss" 
--     return Notifications { umailCount = umailCount,
--                            commentsCount = commentsCount,
--                            discussCount = discussCount,
--                            comments = comments,
--                            umails = umails,
--                            discussions = discussions }


instance FromJSON Notifications where   
  parseJSON = withObject "notifications" $ \n -> do
    umailCount    <- (\x -> read x :: Integer) <$> n .: "umail_count"
    commentsCount <- (\x -> read x :: Integer) <$> n .: "comments_count"
    discussCount  <- (\x -> read x :: Integer) <$> n .: "discuss_count"
    (comments :: J.CommentList)  <- parseJSON =<< n .: "comments"
    (umails :: J.UmailList)  <- parseJSON =<< n .: "umail"  
    (discussions :: J.DiscussionList)  <- parseJSON =<< n .: "discuss" 
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

data Umail = Umail {
    umailid :: Text
  , dateline :: Text
  -- , commentCount :: Text
  , utitle :: Text
  , messageHtml :: Text
  , fromUsername :: Text
  -- , journalname :: Maybe Text 
} deriving (Eq, Show)
        -- count - всего писем, соответствующих параметрам;
        -- umail - набор писем: 

        -- umailid - идентификатор письма, 
        -- from_userid - идентификатор отправителя, 
        -- from_username - логин отправителя, 
        -- dateline - дата-время отправки письма, 
        -- read - флаг прочтения, 
        -- no_smilies - флаг запрета конвертации текстовых смайлов, 
        -- title - тема письма, 
        -- message_html - текст письма. 



instance FromJSON Umail where

  parseJSON (Object v) = Umail <$> v .: "umailid" 
                              <*> v .: "dateline_date" 
                              -- <*> v .: "count"
                              <*> v .: "title"
                              <*> v .: "message_html"
                              <*> v .: "from_username"
                              -- <*> v .:? "journal_name"
  parseJSON _ = mzero

newtype UmailList = UmailList [Umail] deriving (Eq, Show)

instance FromJSON UmailList where
  parseJSON = withObject "umail" $ \u -> do
      umails <- parseJSON' =<< u .: "umail"
      return umails where
          parseJSON' (Object v) = UmailList <$> HMS.foldrWithKey go (pure []) v
              where
                go i x r = (\(Umail _ d t m f) rest -> Umail i d t m f: rest) <$>
                               parseJSON x <*> r
          parseJSON' _ = return $ UmailList []
