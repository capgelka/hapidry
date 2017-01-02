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
    , PostComment(..)
    , PostCommentList(..)
    ) where

import Data.Aeson
import Data.Text (Text, pack)
import Control.Applicative
import Control.Monad (mzero)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M

import Data.Time.Clock.POSIX
import Data.Time.Format

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
  , date :: Text
  , timestamp :: Int
  , commentCount :: Text
  , title :: Text
  , message :: Text
  , shortname :: Text
  , journalname :: Maybe Text 
  , author :: Text
} deriving (Eq, Show)

convertTime :: String -> Text
convertTime timestr =  pack $ formatTime defaultTimeLocale "%c" 
                            $ posixSecondsToUTCTime
                            $ (fromIntegral (read timestr :: Int) :: POSIXTime)

instance FromJSON Post where

  parseJSON (Object v) = Post <$> v .: "postid" 
                              <*> (convertTime <$> timestamp)
                              <*> ((\x -> read x :: Int) <$> timestamp)
                              <*> v .:? "comments_count_data" .!= "0"
                              <*> v .: "title"
                              <*> v .: "message_html"
                              <*> v .: "shortname"
                              <*> v .:? "journal_name"
                              <*> v .: "author_username" where
                                  timestamp = v .: "dateline_date"
  parseJSON _ = mzero

newtype PostList = PostList { posts :: [Post] } deriving (Eq, Show)

instance FromJSON PostList where
  parseJSON = withObject "posts" $
     \p -> parseJSON' =<< p .: "posts" where
          parseJSON' (Object v) = PostList <$> HMS.foldrWithKey go (pure []) v
              where
                go i x r = (\(Post _ d ts c t m s j a) 
                              rest -> Post i d ts c t m s j a: rest) <$> parseJSON
                                                                   x <*> r
          parseJSON' _ = return $ PostList []


data UmailMessage = UmailMessage {
    umailid :: Text
  , dateline :: Text
  , utimestamp :: Int
  , utitle :: Text
  , messageHtml :: Text
  , username :: Text
} deriving (Eq, Show)



instance FromJSON UmailMessage where

  parseJSON (Object v) = UmailMessage <$> v .: "umailid" 
                                      <*> (convertTime <$> timestamp)
                                      <*> ((\x -> read x :: Int) <$> timestamp)
                                      <*> v .: "title"
                                      <*> v .: "message_html"
                                      <*> v .: "from_username" where
                                          timestamp = v .: "dateline"
  parseJSON _ = mzero

newtype MessageList = MessageList { umails :: [UmailMessage] } deriving (Eq, Show)


instance FromJSON MessageList where
    parseJSON (Object v) = do
              x <- v .: "umail"
              MessageList <$> HMS.foldrWithKey go (pure []) x where
                go i x r = (\(UmailMessage _ d ts t m f) rest 
                            -> UmailMessage i d ts t m f: rest) <$>
                                  parseJSON x <*> r
    parseJSON _ = return $ MessageList []

data PostComment = PostComment {
    commentid :: Text
  , cdate :: Text
  , ctimestamp :: Int
  , ctitle :: Text
  , cmessage :: Text
  , cauthor :: Text
} deriving (Eq, Show)

instance FromJSON PostComment where

  parseJSON (Object v) = PostComment <$> v .: "commentid" 
                                      <*> (convertTime <$> timestamp)
                                      <*> ((\x -> read x :: Int) <$> timestamp)
                                      <*> v .: "author_title"
                                      <*> v .: "message_html"
                                      <*> v .: "author_username" where
                                          timestamp = v .: "dateline"
  parseJSON _ = mzero


newtype PostCommentList = PostCommentList { comments :: [PostComment] } deriving (Eq, Show)


instance FromJSON PostCommentList where
    parseJSON (Object v) = do
              x <- v .: "comments"
              PostCommentList <$> HMS.foldrWithKey go (pure []) x where
                go i x r = (\(PostComment _ d ts t m f) rest 
                            -> PostComment i d ts t m f: rest) <$>
                                  parseJSON x <*> r
    parseJSON _ = return $ PostCommentList []