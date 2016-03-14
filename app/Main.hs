{-# LANGUAGE  OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import Control.Lens ((&), (^.), (^?))
import Data.Aeson.Lens (key, _String)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack, map)
import qualified Data.Text.Lazy as L (append, cons, tail, head,
                                      take, drop, Text)
import qualified Data.Text.Lazy.Encoding as LE (decodeUtf8, encodeUtf8)
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Text.Read (decimal)
import qualified Data.ByteString.Base16 as B16 (encode)
import qualified Data.ByteString.Char8 as B
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Lazy.Char8 as BL 
import qualified Data.Map as Map

import Network.Wreq hiding (Auth, auth)
import qualified Network.Wreq.Types as NWTP (FormValue, renderFormValue)
import Options.Applicative

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT



newtype DiaryText = DiaryText Text
instance NWTP.FormValue DiaryText where
    renderFormValue (DiaryText t) = toCP1251 t

-- all data needed for requests to api
data  ClientCredentials =  ClientCredentials {
      password :: B.ByteString,
      appkey   :: Text,
      sid      :: Either Integer Text,
      username :: Text,  
      secret   :: B.ByteString
      } deriving Show


keyHash :: B.ByteString -> B.ByteString -> Text
keyHash pass key = decodeLatin1 $ B16.encode $ MD5.hash $ B.append key pass 
   
ununicode :: BL.ByteString -> BL.ByteString               
ununicode s = LE.encodeUtf8 $ replace $ LE.decodeUtf8 s where 
  replace :: L.Text -> L.Text
  replace "" = ""
  replace string = case (Map.lookup (L.take 6 string) table) of
          (Just x)  -> L.append x (replace $ L.drop 6 string)
          (Nothing) -> L.cons (L.head string) (replace $ L.tail string)

  table = Map.fromList $ zip letters rus

  rus =  ["Ё", "ё", "А", "Б", "В", "Г", "Д", "Е", "Ж", "З", "И", "Й", "К", "Л", "М",
         "Н", "О", "П", "Р", "С", "Т", "У", "Ф", "Х", "Ц", "Ч", "Ш", "Щ", "Ъ", "Ы",
         "Ь", "Э", "Ю", "Я", "а", "б", "в", "г", "д", "е", "ж", "з", "и", "й", "к",
         "л", "м", "н", "о", "п", "р", "с", "т", "у", "ф", "х", "ц", "ч", "ш", "щ",
         "ъ", "ы", "ь", "э", "ю", "я", "—"]  :: [L.Text]
 
  letters = ["\\u0401", "\\u0451", "\\u0410", "\\u0411", "\\u0412", "\\u0413", 
             "\\u0414", "\\u0415", "\\u0416", "\\u0417", "\\u0418", "\\u0419",
             "\\u041a", "\\u041b", "\\u041c", "\\u041d", "\\u041e", "\\u041f",
             "\\u0420", "\\u0421", "\\u0422", "\\u0423", "\\u0424", "\\u0425",
             "\\u0426", "\\u0427", "\\u0428", "\\u0429", "\\u042a", "\\u042b",
             "\\u042c", "\\u042d", "\\u042e", "\\u042f", "\\u0430", "\\u0431",
             "\\u0432", "\\u0433", "\\u0434", "\\u0435", "\\u0436", "\\u0437",
             "\\u0438", "\\u0439", "\\u043a", "\\u043b", "\\u043c", "\\u043d",
             "\\u043e", "\\u043f", "\\u0440", "\\u0441", "\\u0442", "\\u0443",
             "\\u0444", "\\u0445", "\\u0446", "\\u0447", "\\u0448", "\\u0449",
             "\\u044a", "\\u044b", "\\u044c", "\\u044d", "\\u044e", "\\u044f",
             "\\u2014"] :: [L.Text]


toCP1251 :: Text -> B.ByteString
toCP1251 = B.pack . T.unpack . T.map replace where

  replace l = case (Map.lookup l table) of
      (Just x) -> x
      (Nothing) -> l

  table = Map.fromList $ zip rus cpCodes
  cpCodes = map toEnum (168:184:[192 .. 255]) :: [Char]
  rus =  ['Ё', 'ё', 'А', 'Б', 'В', 'Г', 'Д', 'Е', 'Ж', 'З', 'И', 'Й', 'К', 'Л', 'М',
         'Н', 'О', 'П', 'Р', 'С', 'Т', 'У', 'Ф', 'Х', 'Ц', 'Ч', 'Ш', 'Щ', 'Ъ', 'Ы',
         'Ь', 'Э', 'Ю', 'Я', 'а', 'б', 'в', 'г', 'д', 'е', 'ж', 'з', 'и', 'й', 'к',
         'л', 'м', 'н', 'о', 'п', 'р', 'с', 'т', 'у', 'ф', 'х', 'ц', 'ч', 'ш', 'щ',
         'ъ', 'ы', 'ь', 'э', 'ю', 'я']  :: [Char]

toForm :: [(Text, Text)] -> [FormParam]
toForm = map (\(x, y) -> (encodeUtf8 x) := DiaryText y)

apiPost :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer BL.ByteString)
apiPost env p = apiPost' env p where
    updateSid :: Text -> [(Text, Text)] -> [(Text, Text)]
    updateSid sid params = ("sid", sid):(filter (\(x, _) -> x /= "sid") params)
    apiPost' :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer BL.ByteString)
    apiPost' e params = case e & sid of
        (Left _)  -> return $ Left $ (-1)
        (Right x) ->  apiPost'' $ updateSid x params where
            apiPost'' params = do
                r <- post "http://www.diary.ru/api" $ toForm params
                case r ^? responseBody . key "result" . _String of
                   (Just "0")  -> return $ Right $ ununicode $ r ^. responseBody
                   (Just "12") -> authRequest e >>= (\newEnv -> apiPost' newEnv params)
                   (Just x)  -> return $ Left
                                       $ (\y -> case y of
                                              (Right a) -> fst a
                                              (Left _) -> (-1))
                                       $ decimal
                                       $ x 
                   Nothing   -> return $ Left (-1)

postCreate :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer BL.ByteString)
postCreate env p = apiPost env (("method", "post.create"):p)

userGet :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer BL.ByteString)
userGet env params = apiPost env (("method", "user.get"):params)


umailGet :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer BL.ByteString)
umailGet env params = apiPost env (("method", "umail.get"):params)


umailSend :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer BL.ByteString)
umailSend env params = apiPost env (("method", "umail.send"):params)
 


authRequest :: ClientCredentials -> IO ClientCredentials
authRequest env = do
  let params = toForm [("appkey", appkey env),
                       ("password", keyHash (password env) (secret env)),
                       ("username", username env),
                       ("method",  "user.auth")]
  r <- post "http://www.diary.ru/api" params 
             
  return $ env { sid = (authParse r)} where 
    authParse :: Response BL.ByteString -> Either Integer Text
    authParse response = case response  ^? responseBody . key "result" of
            (Just "0") -> Right $ (response ^. responseBody . key "sid" . _String)
            (Just  _)  -> Left $ (\x -> case x of
                                      (Right a) -> fst a
                                      (Left _) -> 0)
                               $ decimal
                               $ (response ^. responseBody . key "result" . _String)
            Nothing    -> Left $ (-1)


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
                appkey  = "6e5f8970828d967595661329239df3b5",
                sid     = Right "",
                username    = username,  
                secret  = "a503505ae803ee7f4fd477f01c1958b1"
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