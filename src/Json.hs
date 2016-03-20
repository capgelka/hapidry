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



data Journal = Journal {
    userid :: Text
  , shortname :: Text
  } deriving (Show, Generic, Eq)


instance FromJSON Journal where   
  parseJSON = withObject "journal" $ \j -> do
    journal <- j .: "journal"
    userid <- pack <$> journal .: "userid"
    shortname <- pack <$> journal .: "shortname"
    return Journal { userid = userid, shortname = shortname }