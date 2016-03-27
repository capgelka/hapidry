{-# LANGUAGE OverloadedStrings #-}

module Internal.Json
    (
      UmailList(..)
    , CommentList(..)
    , DiscussionList(..)
    , Discussion(..)
    , Umail(..)
    , Comment(..)
    ) where

import Data.Aeson
import Data.Text (Text, pack)
import Control.Applicative
import Control.Monad (mzero)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M

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