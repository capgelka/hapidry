{-# LANGUAGE  OverloadedStrings #-}

module Utils 
  ( readOption
  , readOptionB
  , updateCreds
  , createPost
  , sendUmail 
  ) where

import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)

import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Char8 as B

import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Control.Lens ((&))
import Text.Editor (runUserEditor)
import Api

import Options

applyOptions :: [(Text, Maybe String)] -> [(Text, Text)]
applyOptions = map (\(x, Just y) -> (x, T.pack $ y)) . filter (\(_, y) -> y /= Nothing)

updateCreds :: ClientCredentials -> Auth -> ClientCredentials
updateCreds  client (Auth Nothing Nothing)   = client
updateCreds  client (Auth Nothing (Just x))  = client { password = B.pack x } 
updateCreds  client (Auth (Just x) Nothing)  = client { username = T.pack x }
updateCreds  client (Auth (Just x) (Just y)) = client { username = T.pack x,
                                                        password = B.pack y } 

readOption :: CT.Config -> CT.Name -> IO Text
readOption conf opt = (readOption' <$> (C.lookup conf opt) ) where
    readOption' (Just x) = x
    readOption' Nothing  = ""

readOptionB :: CT.Config -> CT.Name -> IO B.ByteString
readOptionB conf opt = encodeUtf8 <$> readOption conf opt

createPost :: Commands -> ClientCredentials -> IO (Either Integer [BL.ByteString])
createPost (Post blogs _ title (Just x) False) client = do
    text <- readFile x
    postsCreate client (applyOptions
                      [("message", Just text),
                       ("title", title)])
                (map T.pack blogs)    
createPost (Post blogs _ title _ True) client = do 
  text <- getContents
  postsCreate client  (applyOptions
                      [("message", Just text),
                       ("title", title)])
              (map T.pack blogs)
createPost (Post blogs Nothing title Nothing False) client = do 
  text <- T.unpack <$> decodeUtf8 <$> runUserEditor
  postsCreate client  (applyOptions
                      [("message", Just text),
                       ("title", title)])
              (map T.pack blogs)
createPost (Post blogs text title _ _) client = postsCreate client
                                                           (applyOptions [("message", text),
                                                                          ("title", title)])
                                                           (map T.pack blogs)



sendUmail :: Commands -> ClientCredentials -> IO (Either Integer BL.ByteString)
sendUmail (Send user _ title (Just x) False) client = do
    text <- readFile x
    umailSend client (applyOptions
                      [("username", Just user),
                       ("message", Just text),
                       ("title", title)])    
sendUmail (Send user _ title _ True) client = do 
    text <- getContents
    umailSend client  (applyOptions
                        [("username", Just user),
                         ("message", Just text),
                         ("title", title)])
sendUmail (Send user Nothing title Nothing False) client = do 
    text <- T.unpack <$> decodeUtf8 <$> runUserEditor
    umailSend client  (applyOptions
                        [("username", Just user),
                         ("message", Just text),
                         ("title", title)])  
sendUmail (Send user text title _ _) client = umailSend client 
                                                    (applyOptions
                                                     [("username", Just user),
                                                      ("message", text),
                                                      ("title", title)])