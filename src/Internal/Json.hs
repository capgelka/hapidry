{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DuplicateRecordFields #-}

module Internal.Json
    (
      UmailList(..)
    , CommentList(..)
    , DiscussionList(..)
    , Discussion(..)
    , Umail(..)
    , Comment(..)
    , Post(..)
    , PostList(..)
    , UmailMessage(..)
    , MessageList(..)
    ) where

import Data.Aeson
import Data.Text (Text, pack)
import Control.Applicative
import Control.Monad (mzero)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import Debug.Trace (trace, traceShow)

type Preview = Text
type Id = Text
type JournalName = Text
type Title = Text
type Username = Text
data Discussion = Discussion JournalName Id Preview deriving (Show, Eq)
data Umail = Umail Username Title Preview deriving (Show, Eq)
data Comment = Comment Id Preview deriving (Show, Eq)


newtype CommentList = CommentList [Comment] deriving (Show, Eq)

instance FromJSON Comment where
  parseJSON (Object v) = Comment <$> v .: "postid" <*> v .: "message_txt"
  parseJSON _ = mzero


instance FromJSON Umail where
  parseJSON (Object v) = Umail <$> v .: "from_username" 
                               <*> v .: "title" 
                               <*> v .: "message_txt"
  parseJSON _ = mzero

instance FromJSON Discussion where
  parseJSON (Object v) = Discussion <$> v .: "journal_name" 
                                    <*> v .: "postid" 
                                    <*> v .: "message_txt"
  parseJSON _ = mzero

instance FromJSON CommentList where
  parseJSON (Object v) = CommentList <$> HMS.foldrWithKey go (pure []) v
    where
      go i x r = (\(Comment pid msg) rest -> Comment i msg : rest) <$>
                     parseJSON x <*> r
  parseJSON _ = return $ CommentList []

newtype UmailList = UmailList [Umail] deriving (Show, Eq)

instance FromJSON UmailList where
  parseJSON (Object v) = UmailList <$> HMS.foldrWithKey go (pure []) v
    where
      go i x r = (\(Umail u t msg) rest -> Umail u t msg : rest) <$>
                     parseJSON x <*> r
  parseJSON _ = return $ UmailList []

newtype DiscussionList = DiscussionList [Discussion] deriving (Show, Eq)

instance FromJSON DiscussionList where
  parseJSON (Object v) = DiscussionList <$> HMS.foldrWithKey go (pure []) v
    where
      go i x r = (\(Discussion j _ msg) rest -> Discussion j i msg : rest) <$>
                     parseJSON x <*> r
  parseJSON _ = return $ DiscussionList []


data Post = Post {
    postid :: Text
  , date :: Int
  , commentCount :: Text
  , title :: Text
  , message :: Text
  , shortname :: Text
  , journalname :: Maybe Text 
} deriving (Eq, Show)


instance FromJSON Post where

  parseJSON (Object v) = Post <$> v .: "postid" 
                              <*> v .: "dateline_date" 
                              <*> v .:? "comments_count_data" .!= "0"
                              <*> v .: "title"
                              <*> v .: "message_html"
                              <*> v .: "shortname"
                              <*> v .:? "journal_name"
  parseJSON _ = mzero

newtype PostList = PostList { posts :: [Post] } deriving (Eq, Show)

instance FromJSON PostList where
  parseJSON = withObject "posts" $ \p -> do
      posts <- parseJSON' =<< p .: "posts"
      return posts where
          parseJSON' (Object v) = PostList <$> HMS.foldrWithKey go (pure []) v
              where
                go i x r = (\(Post _ d c t m s j) rest -> Post i d c t m s j: rest) <$>
                               parseJSON x <*> r
          parseJSON' _ = return $ PostList []


data UmailMessage = UmailMessage {
    umailid :: Text
  , dateline :: Text
  -- , commentCount :: Text
  , utitle :: Text
  , messageHtml :: Text
  , username :: Text
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



instance FromJSON UmailMessage where

  parseJSON (Object v) = UmailMessage <$> v .: "umailid" 
                              <*> v .: "dateline" 
                              -- <*> v .: "count"
                              <*> v .: "title"
                              <*> v .: "message_html"
                              <*> v .: "from_username"
                              -- <*> v .:? "journal_name"
  parseJSON _ = mzero

data MessageList = MessageList [UmailMessage] deriving (Eq, Show)

-- newtype MList = MList [UmailMessage] deriving (Eq, Show)

-- instance FromJSON MList where
--      parseJSON (Object v) = MList <$> HMS.foldrWithKey go (pure []) v
--               where
--                 go i x r = (\(UmailMessage _ d t m f) rest -> UmailMessage i d t m f: rest) <$>
--                                parseJSON x <*> r
--      parseJSON' _ = return $ MList []


instance FromJSON MessageList where
    parseJSON (Object v) = do
              x <- (v .: "umail") 
              MessageList <$> HMS.foldrWithKey go (pure []) x where
                go i x r = (\(UmailMessage _ d t m f) rest -> UmailMessage i d t m f: rest) <$>
                               parseJSON x <*> r
    parseJSON _ = return $ MessageList []

  -- parseJSON = withObject "umail" $ \u -> do
  --     (umails :: MList) <- u .: "umail"
  --     return umails
  --      -- where
  --         parseJSON' (Object v) = MessageList <$> HMS.foldrWithKey go (pure []) v
  --             where
  --               go i x r = (\(UmailMessage _ d t m f) rest -> UmailMessage i d t m f: rest) <$>
  --                              parseJSON x <*> r
  --         parseJSON' _ = return $ MessageList []
