-- Examples of handling for JSON responses
--
-- This library provides several ways to handle JSON responses

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import Control.Lens ((&), (^.), (^?), (.~), set)
import Data.Aeson (FromJSON, fromJSON, decode, eitherDecode)
import Data.Aeson.Encode (encodeToTextBuilder, encodeToBuilder)
import Data.Aeson.Lens (key, _String, _Integer, _Object, members)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T (append, cons, tail, head, take, drop, pack, unpack)
import qualified Data.Text.Lazy as L (toStrict, append, cons, tail, head,
                                      take, drop, Text)
import qualified Data.Text.Lazy.Encoding as LE (decodeLatin1, decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.Builder as LB (toLazyText)
import Data.Text.Encoding (decodeLatin1, decodeUtf8, encodeUtf8)
import Data.Text.Read (decimal)
import Data.Binary (encode)
import qualified Data.ByteString.Base16 as B16 (encode)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (toLazyByteString, int8)
import qualified Crypto.Hash.MD5 as MD5
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as BL --(ByteString)
import Numeric (showHex)
import qualified Data.Map as Map
import qualified Data.Char as Char

import GHC.Generics (Generic)
-- import qualified Control.Exception as E.

import Network.Wreq hiding (Auth, auth)
import qualified Network.Wreq.Types as NWTP (FormValue, renderFormValue, params)
import Options.Applicative
import qualified Data.String.Utils as SU (replace)

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

type Environment a = ReaderT ClientCredentials IO a

keyHash :: B.ByteString -> B.ByteString -> Text
-- keyHash pass key = read $ "\"" ++ show (md5 $ B.append key pass) ++ "\"" ::Text
keyHash pass key = decodeLatin1 $ B16.encode $ MD5.hash $ B.append key pass --need fix  to CP1251
                   -- in case hash of
                   --      (MD5Digest h) -> h
-- Get GHC to derive a FromJSON instance for us.

-- apiGet :: IO (Response B.ByteString)
-- sid :: Reader (\x -> replace (BL.pack x))) 
-- ununicode :: BL.ByteString -> B                -- 
ununicode s = LE.encodeUtf8 $ replace $ LE.decodeUtf8 s where 
  -- replace :: BL.ByteString -> BL.ByteString
  replace "" = ""
  replace str = case (Map.lookup (L.take 6 str) table) of
          (Just x) -> L.append x (replace $ L.drop 6 str)
          (Nothing) -> L.cons (L.head str) (replace $ L.tail str)

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
toCP1251 x = B.pack $ SU.replace rus cpCodes (T.unpack x) where

  -- r = B.foldr (\(x, y) -> B.cons (x r' y) ) "" where
  --   r' l = case (Map.lookup l table) of
  --     (Just x) -> x
  --     (Nothing) -> l

  replace "" = ""
  replace str = case (Map.lookup (head str) table) of
            (Just x) -> B.cons x (replace $ tail str)
            -- _        -> "-3"
            (Nothing) -> B.cons (head str) (replace $ tail str) where

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
    updateSid sid params = ("sid", sid):(filter (\(x, y) -> x /= "sid") params)
    --apiPost' :: ClientCredentials -> [FormParam] -> IO (Either Integer BL.ByteString)
    apiPost' e params = case e & sid of
        (Left _)  -> return $ Left $ (-1)
        -- _ -> return $ Left $ (-1)
        (Right x) ->  apiPost'' $ updateSid x params where
        -- (Right x) ->  apiPost'' $ ("sid" := x):(if length params < 5
        --                                         then params
        --                                         else tail params) where
            -- apiPost'' :: [FormParam] -> IO (Either Integer BL.ByteString)
            apiPost'' params = do
                -- print "Phh"
                -- print params
                -- print "UU(("
                r <- post "http://www.diary.ru/api" $ toForm params
                -- print $ r ^? responseBody 
                -- print $ r ^? responseBody . key "result"
                -- print $ r ^? responseBody . key "result" . _String
                -- print $ r ^? responseBody . key "result" . _Integer
                -- print $ r ^? responseBody . key "result" . _String
                -- print $ r ^? responseBody . _String
                case r ^? responseBody . key "result" . _String of
                   (Just "0")  -> return $ Right $ ununicode $ r ^. responseBody
                   (Just "12") -> authRequest e >>= (\newEnv -> apiPost' newEnv params)
                   (Just x)  -> return $ Left
                                       $ (\x -> case x of
                                              (Right a) -> fst a
                                              (Left a) -> 0)
                                       $ decimal
                                       $ x -- $ x
                   Nothing   -> return $ Left (-1)

postCreate :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer BL.ByteString)
postCreate env p = apiPost env (("method", "post.create"):p)

userGet :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer BL.ByteString)
userGet env params = apiPost env (("method", "user.get"):params)


umailGet :: ClientCredentials -> [(Text, Text)] -> IO (Either Integer BL.ByteString)
umailGet env params = apiPost env (("method", "umail.get"):params)
    -- r <- apiGet env (("method", "umail.get"):params)
    -- x <- return $ case r of
    --       (Right a) -> a
    --       (Left a)  ->  ""
    -- return $ x -- ^? key "count" . _String
    --(decode x :: Maybe Umail)
    -- return $ case r of
    --   (Right x) -> Right $ (decode x :: Maybe Umail )-- >>= (\u -> u & umail >>= return)
    --              -- do :: Maybe
    --              --  u <- decode x
    --              --  return $ u & umail 
    --   --(x & eitherDecode :: Either Umail) & "umail" -- Right $ decode x  -- :: Maybe Umail)
    --   (Left x) -> Left $ x


authRequest :: ClientCredentials -> IO ClientCredentials
authRequest env = do
  --r <- getWith params "http://www.diary.ru/api"
  -- let x = password env + 2
  let params = toForm [("appkey", appkey env),
                       ("password", keyHash (password env) (secret env)),
                       ("username", username env),
                       ("method",  "user.auth")]
  r <- post "http://www.diary.ru/api" params -- >>= return 
  --let r =  
  -- r <- getWith params "http://www.diary.ru/api"                         
  return $ env { sid = (authParse r)} where --authParse $ getWith $ params
    -- authParse :: IO String -> Text
    -- authParse :: IO a -> IO b
    authParse response = case response  ^? responseBody . key "result" of
            (Just "0") -> Right $ (response ^. responseBody . key "sid" . _String)
            (Just  x)  -> Left $ (\x -> case x of
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
-- newtype ConfigPath = ConfigPath { comfigPath :: String } deriving (Show)

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
        pipe :: Maybe Bool
        --   action :: Action
        -- , target :: Target
        -- , file :: Path
        -- , blog :: String
        -- , title :: String
      } deriving (Show)


applyOptions :: [(Text, Maybe String)] -> [(Text, Text)]
applyOptions = map (\(x, Just y) -> (x, T.pack $ y)) . filter (\(x, y) -> y /= Nothing)

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
      -- (Just x) -> x
      -- Nothing  -> ""

readOptionB :: CT.Config -> CT.Name -> IO B.ByteString
readOptionB conf opt = encodeUtf8 <$> readOption conf opt


-- createPost :: ClientCredentials -> Commands -> 
-- createPost (Post text title file pipe)

mainOptParse :: IO ()
mainOptParse = do 
  command <- execParser $ (parseArgs 
                          `withInfo` "diary.ru API client") -- >>= print
  cfg <- C.load [C.Required (command & config)]
  -- lookup c
  password <- readOptionB cfg "password"
  username <- readOption cfg "username"
  client <- authRequest $ ClientCredentials {
                password = password,
                appkey  = "6e5f8970828d967595661329239df3b5",
                sid     = Right "",
                username    = username,  
                secret  = "a503505ae803ee7f4fd477f01c1958b1"
              } & updateCreds $ command & auth
  --  print command    --                      ("target", command & target),
  --                      ("title", command & title)])
  -- print ap
  -- print $ toForm ap
  parseOpt (command & commands) client >>= print where
      --parseOpt ::
      parseOpt (Umail "get" _) client = umailGet client []  -- >>= (\(Right x) -> x ^? key "umail")
      parseOpt (Post text title file pipe)
               client  = postCreate client 
                                    (applyOptions [("message", text),
                                                   -- ("target", target),
                                                   ("title", title)])
      parseOpt _  client              = umailGet client [] --  >>= (\(Right x) -> x ^? key "umail") 
  -- print $ case command of
  --   (Umail "get" _) -> umailGet client [] >>= (\(Right x) -> x ^? key "umail")  --- >>= return -- & members
  --   _               -> umailGet client [] >>= (\(Right x) -> x ^? key "umail") --  >>= return 

parseCommands :: Parser Commands
parseCommands = subparser $
    command "umail" (parseUmail   `withInfo` "get/send umails") <>
    command "user"  (parseUser  `withInfo` "get user info") <>
    command "post"  (parsePost `withInfo` "create new post")

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

parsePost :: Parser Commands
parsePost = Post 
    -- <$> argument str (metavar "TEXT")
    -- <*> (strOption $
    --     short 'b'
    --     <> long "blog"
    --     <> value Nothing
    --     <> metavar "POST_BLOG")
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
    <*> (optional $ switch
      (long "pipe"
       <> short 'p'
       <> help "get text from stdout"))
    -- <> value False
--    <> help "Retain all intermediate temporary files" )

main :: IO ()
main = do
  -- print $ toCP1251 "абвг0абвг&abcd"
             
  let client = ClientCredentials {
                password = "1234123",
                appkey  = "6e5f8970828d967595661329239df3b5",
                sid     = Right "",
                username    = "hastest",  
                secret  = "a503505ae803ee7f4fd477f01c1958b1"
             }
  print "OK"
  mainOptParse

  -- r <- authRequest client
  -- print r
  -- print "\\u041d\\u0430\\u0438\\u0431\\u043e\\u043b\\"
  -- print $ ununicode "\\u041d\\u0430\\u0438\\u0431\\u043e\\u043b\\"
  -- let letters = BL.concat $ ["\\u0401", "\\u0451", "\\u0410", "\\u0411", "\\u0412", "\\u0413", 
  --                            "\\u0414", "\\u0415", "\\u0416", "\\u0417", "\\u0418", "\\u0419",
  --                            "\\u041a", "\\u041b", "\\u041c", "\\u041d", "\\u041e", "\\u041f",
  --                            "\\u0420", "\\u0421", "\\u0422", "\\u0423", "\\u0424", "\\u0425",
  --                            "\\u0426", "\\u0427", "\\u0428", "\\u0429", "\\u042a", "\\u042b",
  --                            "\\u042c", "\\u042d", "\\u042e", "\\u042f", "\\u0430", "\\u0431",
  --                            "\\u0432", "\\u0433", "\\u0434", "\\u0435", "\\u0436", "\\u0437",
  --                            "\\u0438", "\\u0439", "\\u043a", "\\u043b", "\\u043c", "\\u043d",
  --                            "\\u043e", "\\u043f", "\\u0440", "\\u0441", "\\u0442", "\\u0443",
  --                            "\\u0444", "\\u0445", "\\u0446", "\\u0447", "\\u0448", "\\u0449",
  --                            "\\u044a", "\\u044b", "\\u044c", "\\u044d", "\\u044e", "\\u044f"]

  -- print letters
  -- r2 <- umailGet client []
  -- -- print $ r2
  -- r3 <- apiGet client (("method", "umail.get"):[])
  -- let z = ((\(Right x) -> x) r2) ^? key "count" 
  -- print $ ((\(Right x) -> x) r2) ^? key "umail"  -- . _Object
  -- -- print r3
  -- putStrLn $ BL.unpack r2
  -- let p = "\\u0414\\u043e\\u0431\\u0440\\u043e \\u043f\\u043e\\u0436 Diary"
  -- print p
  -- putStrLn $ B.unpack $ ununicode' $ p
  -- print $ r ^? responseBody . key "sid"
  -- -- (authorize appkey skey "hastest" "12341230") + 2
  -- a <- authorize appkey skey "hastest" "12341230"
  -- print $ a
  -- aa <- authorize appkey skey "hastest" "1234123"
  -- print $ aa
  -- print v
  -- print authRequest
  -- lens_aeson
