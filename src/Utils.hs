{-# LANGUAGE  OverloadedStrings, CPP, DuplicateRecordFields #-}

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
  , getResponseField
  , printError
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
#ifdef linux_HOST_OS
import Text.Editor (runUserEditor)
#endif
import Data.Maybe (isJust, maybeToList, fromMaybe)
import Api
import Data.Aeson
import Json
import qualified Internal.Json as IJ --(MessageList, UmailMessage)
import qualified Json as J
import Options
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Char (isDigit)
import System.Directory
import System.Environment(lookupEnv)

import Data.Aeson.Lens (key, _String)
import Control.Lens ((&), (^.))

import System.Exit

import Debug.Trace

readSid :: Text -> IO Text
readSid username = do 
  path <- T.unpack <$> (getTempPrefix >>= \tmp -> return $ T.concat [tmp, "/hapidry_", username])
  exist <- doesFileExist path
  if exist then T.readFile path else return "" where
          getTempPrefix :: IO Text
          getTempPrefix = T.pack . fromMaybe "/tmp" <$> lookupEnv "TEMP"


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

createPost :: Commands -> ClientCredentials -> IO (Either BL.ByteString [BL.ByteString])
createPost (Post blogs _ title (Just x) False whitelist draft tags music mood) client = do
    text <- readFile x
    postsCreate client (applyOptions
                      [("message", Just text),
                       ("title", title),
                       ("close_access_mode", Just $ if whitelist then "4" else "0"),
                       ("type",  if draft then Just "draft" else Nothing),
                       ("current_music", music),
                       ("current_mood", mood)] 
                       ++ convertTags tags)
                (map T.pack blogs)    
createPost (Post blogs _ title _ True whitelist draft tags music mood) client = do 
  text <- getContents
  postsCreate client  (applyOptions
                      [("message", Just text),
                       ("title", title),
                       ("close_access_mode", Just $ if whitelist then "4" else "0"),
                       ("type",  if draft then Just "draft" else Nothing),
                       ("current_music", music),
                       ("current_mood", mood)]  
                       ++ convertTags tags)
              (map T.pack blogs)
#ifdef linux_HOST_OS
createPost (Post blogs Nothing title Nothing False whitelist draft tags music mood) client = do 
  text <- T.unpack <$> decodeUtf8 <$> runUserEditor
  postsCreate client  (applyOptions
                      [("message", Just text),
                       ("title", title),
                       ("close_access_mode", Just $ if whitelist then "4" else "0"),
                       ("type",  if draft then Just "draft" else Nothing),
                       ("current_music", music),
                       ("current_mood", mood)] 
                       ++ convertTags tags)
              (map T.pack blogs)
#endif
createPost (Post blogs text title _ _ whitelist draft tags music mood) client = postsCreate client
                               (applyOptions [("message", text),
                                              ("title", title),
                                              ("close_access_mode", 
                                               Just $ if whitelist then "4" else "0"),
                                               ("type",  
                                                if draft then Just "draft" else Nothing),
                                              ("current_music", music),
                                              ("current_mood", mood)] 
                                ++ convertTags tags)
                               (map T.pack blogs)



sendUmail :: Commands -> ClientCredentials -> IO (Either BL.ByteString [BL.ByteString])
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
#ifdef linux_HOST_OS               
sendUmail (Send users Nothing title Nothing False) client = do 
    text <- T.unpack <$> decodeUtf8 <$> runUserEditor
    umailsSend client (applyOptions
                        [("message", Just text),
                         ("title", title)])
               (map T.pack users)
#endif               
sendUmail (Send users text title _ _) client = umailsSend client 
                                                          (applyOptions [("message", text),
                                                                         ("title", title)]) 
                                                          (map T.pack users)

createComment :: Commands -> ClientCredentials -> IO (Either BL.ByteString [BL.ByteString])
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
#ifdef linux_HOST_OS             
createComment (Comment pid Nothing Nothing False) client = do 
    text <- T.unpack <$> decodeUtf8 <$> runUserEditor
    sequence <$> (: []) 
             <$> commentCreate client (applyOptions [("message", Just text),
                                                     ("postid", Just pid)]) 
#endif             
createComment (Comment pid text  _ _) client = sequence <$> (: [])
                                                        <$> commentCreate 
                                                            client 
                                                            (applyOptions [("message", text),
                                                                           ("postid", Just pid)])
                                                              
readPost :: Commands -> ClientCredentials -> IO ()
readPost b@(Blog blognames order) client | length blognames /= 1     = readPost' b
                                         | isPostId $ head blognames = readComments $ head blognames 
                                         | otherwise                 = readPost' b where

    isPostId :: String -> Bool
    isPostId ('p':xs) = all isDigit xs
    isPostId xs       = all isDigit xs

    readPost' :: Commands -> IO ()
    readPost' (Blog blognames order) = do
      let proc = if order then id else reverse
      result <- postsFromJson <$> postsGet client [] (map T.pack blognames)
      posts <- case result of
        (Right x) -> return x
        (Left x)  -> printError x >> return []
      let sorted = proc $ sortBy (comparing (& IJ.timestamp)) 
                                  posts
      mapM_ printBlog sorted where
        printBlog :: IJ.Post -> IO ()
        printBlog p = do
          date <- p & IJ.date
          T.putStrLn $ case p & IJ.journalname of
              Just x -> T.concat ["<h3>", x, " (", p & IJ.author, ")</h3><br>"]
              Nothing -> ""
          T.putStrLn $ T.concat [date,
                                ": ",
                                 p & IJ.title, 
                                 " [",
                                 p & IJ.postid,
                                 "]"]
          T.putStrLn "<br><br>\n\n"
          T.putStrLn $ p & IJ.message
          T.putStrLn $ T.concat ["comments: ", p & IJ.commentCount, "<br><br><br>\n\n\n"]


    readComments :: String -> IO ()
    readComments post = do 
      comments <- commentsFromJson <$> commentsGet client [] (T.pack post)
      let proc = if order then reverse else id
      let sorted = proc $ sortBy (comparing (& IJ.ctimestamp)) 
                                  comments
      mapM_ printComment sorted where
        printComment :: IJ.PostComment -> IO ()
        printComment c = do
          date <- c & IJ.cdate
          T.putStrLn $ T.concat ["<b>", c & IJ.cauthor, "</b> ",
                                 date, " [", c & IJ.commentid, "]", "<br>"]
          T.putStrLn $ c & IJ.ctitle
          T.putStrLn "<br>"
          T.putStrLn "<br><br>\n\n"
          T.putStrLn $ c & IJ.cmessage

readUmail :: Commands -> ClientCredentials -> IO ()
readUmail (Umail folder order) client = do
  let proc = if order then id else reverse
  let folderType = T.pack $ show $ case folder of
                                    (Just x)  -> 1 + fromEnum x
                                    Nothing   -> 1 + fromEnum Input   
  umails <- umailsFromJson <$> umailGet client [("folder", folderType)]
  let sorted = proc $ sortBy (comparing (& IJ.utimestamp)) 
                              umails
  mapM_ printUmail sorted where
    printUmail :: IJ.UmailMessage -> IO ()
    printUmail u = do
      date <- u & IJ.dateline
      T.putStr $ T.concat ["<h3>(", u & IJ.username, ") "]
      T.putStrLn $ T.concat [date,
                            ": ",
                             u & IJ.utitle, 
                             " [",
                             u & IJ.umailid,
                             "]</h3>"]
      T.putStrLn "<br><br>\n\n"
      T.putStrLn $ IJ.messageHtml u
      T.putStrLn "<br>"

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
            if uc > 0 then T.putStrLn $ T.concat ["you have ", T.pack $ show uc, " unread umails"]
                      else T.putStr ""
            if cc > 0 then T.putStrLn $ T.concat ["you have ", T.pack $ show cc, " unread comments"]
                      else T.putStr ""
            if dc > 0 then T.putStrLn $ T.concat ["you have ", T.pack $ show dc, " unread discussions"]
                      else T.putStr ""


notificationsFromJson :: Either BL.ByteString BL.ByteString -> Maybe Notifications
notificationsFromJson (Right json) = decode json
notificationsFromJson (Left _)     = Nothing


printError :: BL.ByteString -> IO ()
printError json = case decode json of
   (Just r) -> do
      T.putStr "Ошибка: "
      T.putStrLn (r & J.errorText)
      exitWith $ ExitFailure (r & J.returnCode)
   Nothing  -> T.putStrLn "Unknown Error!" >> (exitWith $ ExitFailure (-1))

postsFromJson :: Either BL.ByteString [BL.ByteString] -> Either BL.ByteString [IJ.Post]
postsFromJson (Right x) = Right $ concatMap (\j -> case decode j of
                                            (Just json) -> IJ.posts json
                                            Nothing     -> [])
                                    x
postsFromJson (Left x)  = Left x

umailsFromJson :: Either BL.ByteString BL.ByteString -> [IJ.UmailMessage]
umailsFromJson (Right json) = case decode json of
                               (Just j) -> IJ.umails j
                               Nothing -> []
umailsFromJson (Left x)     =  []

commentsFromJson :: Either BL.ByteString BL.ByteString -> [IJ.PostComment]
commentsFromJson (Right json) = case decode json of
                               (Just c) -> IJ.comments c
                               Nothing -> []
commentsFromJson (Left x)     =  []

getFieldFromJson :: Text -> BL.ByteString -> BL.ByteString
getFieldFromJson field = BL.pack . T.unpack . (\el -> el ^. key field . _String)

getResponseField :: Text -> Either BL.ByteString [BL.ByteString] -> Either BL.ByteString [BL.ByteString]
getResponseField field = \x -> x >>= (\y -> Right $ map (getFieldFromJson field) y)
