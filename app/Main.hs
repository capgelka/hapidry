{-# LANGUAGE  OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}


import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)

import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Char8 as B

import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)

-- import Network.Wreq hiding (Auth, auth)
import Options.Applicative

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Control.Lens ((&))
import Text.Editor (runUserEditor)
import Api




type Action = String
type Target = String
type UserId = String
type Path   = String
data Auth   = Auth (Maybe String) (Maybe String) deriving (Show)
type ConfigPath = String

data Args = Args { auth :: Auth, config :: ConfigPath, commands :: Commands } deriving (Show)

data Commands 
    = Umail  {
        umailAction :: Action
      , target :: Maybe Target
      }
    | User
      {
        userAction :: Action
      , uid :: UserId
      } 
    | Post 
      {
        text :: Maybe String,
        title :: Maybe String,
        file :: Maybe Path,
        pipe :: Bool
      }
    | Send
      {
        user :: String,
        title :: Maybe String,
        text :: Maybe String,
        file :: Maybe Path,
        pipe :: Bool
      } deriving (Show)


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


createPost :: Commands -> ClientCredentials -> IO (Either Integer BL.ByteString)
createPost (Post _ title (Just x) False) client = do
    text <- readFile x
    postCreate client (applyOptions
                      [("message", Just text),
                       ("title", title)])    
createPost (Post _ title _ True) client = do 
  text <- getContents
  postCreate client  (applyOptions
                      [("message", Just text),
                       ("title", title)])
createPost (Post Nothing title Nothing False) client = do 
  text <- T.unpack <$> decodeUtf8 <$> runUserEditor
  postCreate client  (applyOptions
                      [("message", Just text),
                       ("title", title)])
createPost (Post text title _ _) client = postCreate client 
                                                    (applyOptions
                                                     [("message", text),
                                                      ("title", title)])


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
 

mainOptParse :: IO ()
mainOptParse = do 
  command <- execParser $ (parseArgs 
                          `withInfo` "diary.ru API client") 
  cfg <- C.load [C.Required (command & config)]
  password <- readOptionB cfg "password"
  username <- readOption cfg "username"
  client <- authRequest $ ClientCredentials {
                password = password,
                appkey  = "5ab793910e36584cd81622e5eb77d3d1",
                sid     = Right "",
                username    = username,  
                secret  = "8543db8deccb4b0fcb753291c53f8f4f"
              } & updateCreds $ command & auth
  print command                     
  parseOpt (command & commands) client >>= print where
      parseOpt :: Commands -> ClientCredentials -> IO (Either Integer BL.ByteString)
      parseOpt (Umail "get" _) client = umailGet client []  
      parseOpt p@(Post _ _ _ _) client = createPost p client
      parseOpt s@(Send _ _ _ _ _) client = sendUmail s client
      parseOpt _  client              = umailGet client [] 

parseCommands :: Parser Commands
parseCommands = subparser $
    command "umail" (parseUmail `withInfo` "get/send umails") <>
    command "user"  (parseUser  `withInfo` "get user info") <>
    command "post"  (parsePost  `withInfo` "create new post") <>
    command "send"  (parseSend  `withInfo` "send new umail")

parseArgs :: Parser Args
parseArgs = Args <$> parseAuth <*> parseConfig <*> parseCommands

parseConfig :: Parser ConfigPath
parseConfig = (strOption $
              short 'c'
              <> long "config"
              <> value "$(HOME)/.hapidry"
              <> metavar "CONFIG"
              <> help "path to config file")

parseAuth :: Parser Auth
parseAuth = Auth <$> (optional $ strOption $
              short 'u'
              <> long "user"
              <> metavar "LOGIN"
              <> help "user login")
            <*> (optional $ strOption $
                 short 'p'
                 <> long "password"
                 <> metavar "PASSWORD"
                 <> help "user password")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseUser :: Parser Commands
parseUser = User 
    <$> argument str (metavar "USER_ACTION")
    <*> argument str (metavar "USER_UID")


parseUmail :: Parser Commands
parseUmail = Umail 
    <$> argument str (metavar "UMAIL_ACTION")
    <*> (optional $ strOption $
        short 'U'
        <> long "user"
        <> metavar "UMAIL_TARGET")

parseSend :: Parser Commands
parseSend = Send
    <$> argument str (metavar "UMAIL_USERNAME")
    <*> (optional $ strOption $
        short 'm'
        <> long "message"
        <> metavar "UMAIL_MESSAGE")
    <*> (optional $ strOption $
        short 't'
        <> long "title"
        <> metavar "UMAIL_MESSAGE_TITLE")
    <*> (optional $ strOption $
        short 'f'
        <> long "file"
        <> metavar "UMAIL_MESSAGE_FILE")
    <*> (switch
      (long "pipe"
       <> short 'p'
       <> help "get text from stdout"))

parsePost :: Parser Commands
parsePost = Post 
    <$> (optional $ strOption $
        short 'm'
        <> long "message"
        <> metavar "POST_MESSAGE")
    <*> (optional $ strOption $
        short 't'
        <> long "title"
        <> metavar "POST_MESSAGE_TITLE")
    <*> (optional $ strOption $
        short 'f'
        <> long "file"
        <> metavar "POST_MESSAGE_FILE")
    <*> (switch
      (long "pipe"
       <> short 'p'
       <> help "get text from stdout"))


main :: IO ()
main = do

  print "OK"
  mainOptParse