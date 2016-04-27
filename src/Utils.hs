{-# LANGUAGE  OverloadedStrings #-}

module Utils 
  ( readOption
  , readOptionB
  , updateCreds
  , createPost
  , sendUmail
  , getNotifications
  , createComment 
  , postsFromJson --delete later
  , umailsFromJson
  ) where

import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T (pack, unpack, concat)

import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Char8 as B

import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Control.Lens ((&))
-- import Text.Editor (runUserEditor)
import Data.Maybe (isJust)
import Api
import Data.Aeson
import Json
import qualified Internal.Json as IJ --(MessageList, UmailMessage)
import Options

applyOptions :: [(Text, Maybe String)] -> [(Text, Text)]
applyOptions = map (\(x, Just y) -> (x, T.pack y)) . filter (\(_, y) -> isJust y)

updateCreds :: ClientCredentials -> Auth -> ClientCredentials
updateCreds  client (Auth Nothing Nothing)   = client
updateCreds  client (Auth Nothing (Just x))  = client { password = B.pack x } 
updateCreds  client (Auth (Just x) Nothing)  = client { username = T.pack x }
updateCreds  client (Auth (Just x) (Just y)) = client { username = T.pack x,
                                                        password = B.pack y } 
convertTags :: [String] -> [(Text, Text)]
convertTags = map (\t -> ("tags_data[]", T.pack t))

readOption :: CT.Config -> CT.Name -> IO Text
readOption conf opt = readOption' <$> C.lookup conf opt where
    readOption' (Just x) = x
    readOption' Nothing  = ""

readOptionB :: CT.Config -> CT.Name -> IO B.ByteString
readOptionB conf opt = encodeUtf8 <$> readOption conf opt

createPost :: Commands -> ClientCredentials -> IO (Either Integer [BL.ByteString])
createPost (Post blogs _ title (Just x) False tags) client = do
    text <- readFile x
    postsCreate client (applyOptions
                      [("message", Just text),
                       ("title", title)] ++ convertTags tags)
                (map T.pack blogs)    
createPost (Post blogs _ title _ True tags) client = do 
  text <- getContents
  postsCreate client  (applyOptions
                      [("message", Just text),
                       ("title", title)] ++ convertTags tags)
              (map T.pack blogs)
-- createPost (Post blogs Nothing title Nothing False tags) client = do 
--   text <- T.unpack <$> decodeUtf8 <$> runUserEditor
--   postsCreate client  (applyOptions
--                       [("message", Just text),
--                        ("title", title)] ++ convertTags tags)
--               (map T.pack blogs)
createPost (Post blogs text title _ _ tags) client = postsCreate client
                                                           (applyOptions [("message", text),
                                                                          ("title", title)]
                                                            ++ convertTags tags)
                                                           (map T.pack blogs)



sendUmail :: Commands -> ClientCredentials -> IO (Either Integer [BL.ByteString])
sendUmail (Send users _ title (Just x) False) client = do
    text <- readFile x
    umailsSend client (applyOptions
                      [("message", Just text),
                       ("title", title)])
               (map T.pack users)
sendUmail (Send users _ title _ True) client = do 
    text <- getContents
    umailsSend client (applyOptions
                        [("message", Just text),
                         ("title", title)])
               (map T.pack users)
-- sendUmail (Send users Nothing title Nothing False) client = do 
--     text <- T.unpack <$> decodeUtf8 <$> runUserEditor
--     umailsSend client (applyOptions
--                         [("message", Just text),
--                          ("title", title)])
--                (map T.pack users)
sendUmail (Send users text title _ _) client = umailsSend client 
                                                          (applyOptions [("message", text),
                                                                         ("title", title)]) 
                                                          (map T.pack users)

createComment :: Commands -> ClientCredentials -> IO (Either Integer [BL.ByteString])
createComment (Comment pid _ (Just x) False) client = do
    text <- readFile x
    sequence <$> (: []) 
             <$> commentCreate client (applyOptions [("message", Just text),
                                                     ("postid", Just pid)]) 
createComment (Comment pid _ _ True) client = do 
    text <- getContents
    sequence <$> (: []) 
             <$> commentCreate client (applyOptions [("message", Just text),
                                                     ("postid", Just pid)]) 
-- createComment (Comment pid Nothing Nothing False) client = do 
--     text <- T.unpack <$> decodeUtf8 <$> runUserEditor
--     sequence <$> (: []) 
--              <$> commentCreate client (applyOptions [("message", Just text),
--                                                      ("postid", Just pid)]) 
createComment (Comment pid text  _ _) client = sequence <$> (: [])
                                                        <$> commentCreate 
                                                            client 
                                                            (applyOptions [("message", text),
                                                                           ("postid", Just pid)])
                                                              
                                                                                                    

getNotifications :: Commands -> ClientCredentials -> IO ()
getNotifications opt client = do 
  notifications <- notificationsFromJson <$> notificationGet client []
  case notifications of
      Nothing  -> error "error can't read notifications"
      (Just n) -> getNotifications' opt n where
          getNotifications' :: Commands -> Notifications -> IO ()
          -- getNotifications' (Notify q c umails comments disk a)  
          getNotifications' _ nt = do
            let uc = nt & umailCount
            let cc = nt & commentsCount
            let dc =  nt & discussCount
            if uc > 0 then T.putStrLn $ T.concat ["you have ", (T.pack $ show uc), " unread umails"]
                      else T.putStr ""
            if cc > 0 then T.putStrLn $ T.concat ["you have ", (T.pack $ show cc), " unread comments"]
                      else T.putStr ""
            if dc > 0 then T.putStrLn $ T.concat ["you have ", (T.pack $ show dc), " unread discussions"]
                      else T.putStr ""


notificationsFromJson :: Either Integer BL.ByteString -> Maybe Notifications
notificationsFromJson (Right json) = decode json
notificationsFromJson (Left _)     = Nothing


postsFromJson :: Either Integer BL.ByteString -> Maybe PostList
postsFromJson (Right json) = decode json
postsFromJson (Left x)     = Just $ PostList []

umailsFromJson :: Either Integer BL.ByteString -> Maybe IJ.MessageList
umailsFromJson (Right json) = decode json
umailsFromJson (Left x)     = Just $ IJ.MessageList []