{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Json
    (
      Journal(..)
    , Notifications(..)
    , J.PostList(..)
    , ApiError(..)
    ) where

import Data.Aeson
import Data.Text (Text, pack)
import Control.Applicative
import Control.Monad
import qualified Data.HashMap.Strict as HMS
import qualified Internal.Json as J

data ApiError = ApiError {
    errorText   :: Text
  , returnCode  :: Int
  } deriving (Show, Eq)

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

instance FromJSON ApiError where   
  parseJSON (Object ae) = ApiError <$> ae .: "error" 
                                   <*> ((\x -> read x :: Int) <$> ae .: "result" )
  parseJSON _ = mzero
