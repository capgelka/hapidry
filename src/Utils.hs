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
  , readPost
  , readUmail
  , readSid
  ) where

import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T (pack, unpack, concat, append)

import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Char8 as B

import qualified Data.ByteString.Lazy.Char8 as BL --(ByteString)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Control.Lens ((&))
import Text.Editor (runUserEditor)
import Data.Maybe (isJust, maybeToList)
import Api
import Data.Aeson
import Json
import qualified Internal.Json as IJ --(MessageList, UmailMessage)
import Options
import Data.List (sortBy)
import Data.Ord (comparing)
import System.Directory

-- import Data.Time.Format
-- import Data.Time.Clock.POSIX
import qualified System.Locale as SL

import Debug.Trace

readSid :: Text -> IO Text
readSid username = do 
  let path = T.unpack $ T.concat ["/tmp/hapidry_", username]
  exist <- doesFileExist path
  if exist then T.readFile path else return ""


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
createPost (Post blogs _ title (Just x) False whitelist draft tags) client = do
    text <- readFile x
    postsCreate client (applyOptions
                      [("message", Just text),
                       ("title", title),
                       ("close_access_mode", Just $ if whitelist then "4" else "0"),
                       ("type",  if draft then Just "draft" else Nothing)] 
                       ++ convertTags tags)
                (map T.pack blogs)    
createPost (Post blogs _ title _ True whitelist draft tags) client = do 
  text <- getContents
  postsCreate client  (applyOptions
                      [("message", Just text),
                       ("title", title),
                       ("close_access_mode", Just $ if whitelist then "4" else "0"),
                       ("type",  if draft then Just "draft" else Nothing)] 
                       ++ convertTags tags)
              (map T.pack blogs)
createPost (Post blogs Nothing title Nothing False whitelist draft tags) client = do 
  text <- T.unpack <$> decodeUtf8 <$> runUserEditor
  postsCreate client  (applyOptions
                      [("message", Just text),
                       ("title", title),
                       ("close_access_mode", Just $ if whitelist then "4" else "0"),
                       ("type",  if draft then Just "draft" else Nothing)]
                       ++ convertTags tags)
              (map T.pack blogs)
createPost (Post blogs text title _ _ whitelist draft tags) client = postsCreate client
                               (applyOptions [("message", text),
                                              ("title", title),
                                              ("close_access_mode", 
                                               Just $ if whitelist then "4" else "0"),
                                               ("type",  
                                                if draft then Just "draft" else Nothing)]
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
sendUmail (Send users Nothing title Nothing False) client = do 
    text <- T.unpack <$> decodeUtf8 <$> runUserEditor
    umailsSend client (applyOptions
                        [("message", Just text),
                         ("title", title)])
               (map T.pack users)
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
createComment (Comment pid Nothing Nothing False) client = do 
    text <- T.unpack <$> decodeUtf8 <$> runUserEditor
    sequence <$> (: []) 
             <$> commentCreate client (applyOptions [("message", Just text),
                                                     ("postid", Just pid)]) 
createComment (Comment pid text  _ _) client = sequence <$> (: [])
                                                        <$> commentCreate 
                                                            client 
                                                            (applyOptions [("message", text),
                                                                           ("postid", Just pid)])
                                                              
                                                                                                    

readPost :: Commands -> ClientCredentials -> IO ()
readPost (Blog blognames order) client = do
  let proc = if order then id else reverse
  posts <- postsFromJson <$> postsGet client [] (map T.pack blognames)
  let sorted = proc $ sortBy (comparing (& IJ.timestamp)) 
                              posts
  mapM_ printBlog sorted where
    printBlog :: IJ.Post -> IO ()
    printBlog p = do
      T.putStrLn $ case (p & IJ.journalname) of
          Just x -> T.concat ["<h3>", x, " (", p & IJ.author, ")</h3><br>"]
          Nothing -> ""
      T.putStrLn $ T.concat [p & IJ.date,
                            ": ",
                             p & IJ.title, 
                             " [",
                             p & IJ.postid,
                             "]"]
      T.putStrLn "<br><br>\n\n"
      T.putStrLn $ p & IJ.message
      T.putStrLn $ T.concat $ ["comments: ", (p & IJ.commentCount), "<br><br><br>\n\n\n"]

readUmail :: Commands -> ClientCredentials -> IO ()
readUmail (Umail folder order) client = do
  let proc = if order then id else reverse
  let folderType = T.pack $ show $ case folder of
                                    (Just x)  -> 1 + fromEnum x
                                    (Nothing) -> 1 + fromEnum Input   
  umails <- umailsFromJson <$> umailGet client [("folder", folderType)]
  let sorted = proc $ sortBy (comparing (& IJ.utimestamp)) 
                              umails
  mapM_ printUmail sorted where
    printUmail :: IJ.UmailMessage -> IO ()
    printUmail u = do
      T.putStr $ T.concat ["<h3>(", u & IJ.username, ") "]
      T.putStrLn $ T.concat [u & IJ.dateline,
                            ": ",
                             u & IJ.utitle, 
                             " [",
                             u & IJ.umailid,
                             "]</h3>"]
      T.putStrLn "<br><br>\n\n"
      T.putStrLn $ IJ.messageHtml u
      T.putStrLn $ "<br>"

getNotifications :: Commands -> ClientCredentials -> IO ()
getNotifications opt client = do 
  notifications <- notificationsFromJson <$> notificationGet client []
  case notifications of
      Nothing  -> error "error can't read notifications"
      (Just n) -> getNotifications' opt n where
          getNotifications' :: Commands -> Notifications -> IO ()
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


postsFromJson :: Either Integer [BL.ByteString] -> [IJ.Post]
postsFromJson (Right x) = concatMap (\j -> case (decode j) of
                                            (Just json) -> IJ.posts json
                                            Nothing     -> [])
                                    x
postsFromJson (Left x)  =  []

umailsFromJson :: Either Integer BL.ByteString -> [IJ.UmailMessage]
umailsFromJson (Right json) = case (decode json) of
                               (Just j) -> IJ.umails j
                               Nothing -> []
umailsFromJson (Left x)     =  []
