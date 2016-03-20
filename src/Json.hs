{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Json
    (
      Journal(..)
    -- , JJournal(..)

    ) where

import Data.Aeson.Lens (key, _String)
import Data.Aeson
import Data.Text (Text, pack)
import GHC.Generics
import Control.Applicative

-- {
--     "name":"Nightfall",
--     "author":{
--         "name":"Isaac Asimov",
--         "born":1920
--     }
-- }

-- However, you don't like nested records and you want to convert it into this:

-- data Story = Story {
--   name       :: String,
--   author     :: String,
--   authorBorn :: Int }

-- data JJournal = JJournal { journal :: Journal} deriving (Show, Generic, Eq)


data Journal = Journal {
    userid :: Text
  , shortname :: Text
  } deriving (Show, Generic, Eq)

-- Get GHC to derive a FromJSON instance for us.

-- instance FromJSON JJournal where
--     parseJSON = withObject "jjournal" $ \j -> do
--         journal <- j .: "journal"
--         -- userid <- pack <$> journal .: "userid"
--         -- shortname <- pack <$> journal .: "shortname"
--         --return $ Journal <*> journal .: "userid" <*> journal .: "shortname"
--         return JJournal { journal = journal}

instance FromJSON Journal where   
  parseJSON = withObject "journal" $ \j -> do
    journal <- j .: "journal"
    userid <- pack <$> journal .: "userid"
    shortname <- pack <$> journal .: "shortname"
    --return $ Journal <*> journal .: "userid" <*> journal .: "shortname"
    return Journal { userid = userid, shortname = shortname }