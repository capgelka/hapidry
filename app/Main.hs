{-# LANGUAGE  OverloadedStrings, TemplateHaskell, DuplicateRecordFields, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C (Config)
import Control.Lens ((&))
import Api
import Internal.Api (authRequest)

import Options
import Utils


-- import Prelude hiding (putStr)
-- import Data.ByteString.Char8 (putStr, pack)


import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString, pack, unpack, putStr, putStrLn, concat)
import qualified Data.ByteString.Char8 as B

import Data.Version (showVersion)
import Development.GitRev (gitHash)
import qualified Paths_hapidry as P (version)

import System.Process (callProcess)
import System.Exit
import Control.Exception
import System.IO (stdin, stdout, hGetEcho, hFlush, hSetEcho)


type Delimeter = T.Text

errorMessage :: T.Text -> T.Text
errorMessage m = T.concat ["Конфигурационный файл не существует! Создайте ",
                           m,
                           " вручную или используйте hapidry-generate"]

askPassword :: T.Text -> BL.ByteString -> IO (Either BL.ByteString B.ByteString)
askPassword username err = (encodeUtf8 <$>) <$> askPassword' username where

    getPassword :: T.Text -> IO T.Text
    getPassword username = do
      putStr ("Enter password for " ++ T.unpack username ++ ": ")
      hFlush stdout
      pass <- withEcho False T.getLine
      putChar '\n'
      return pass

    withEcho :: Bool -> IO a -> IO a
    withEcho echo action = do
      old <- hGetEcho stdin
      bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

    askPassword' :: T.Text -> IO (Either BL.ByteString T.Text)
    askPassword' u =  do
      pass <- getPassword u
      return (if T.length pass == 0
              then Left err
              else Right pass)

printResult :: Either BL.ByteString [BL.ByteString] -> Delimeter -> IO ()
printResult (Left x)  _ = printError x
printResult (Right xs) d = mapM_ (\x -> BL.putStr x >> T.putStr d) xs >> putStrLn ""

getApiEndpointRootUrl :: C.Config -> IO String
getApiEndpointRootUrl conf = T.unpack <$> choice <$> (readOption conf "insecure") where
  choice "True" = "http://diary.ru/api"
  choice "true" = "http://diary.ru/api"
  choice _      = "https://diary.ru/api"


getCreds :: Args -> IO ClientCredentials
getCreds command = do
  cfg <- handle (\(e :: IOException) 
                 -> T.putStrLn (errorMessage $ T.pack (command & config)) >> exitWith (ExitFailure 2))
                (C.load [C.Required (command & config)])
  password <- readOptionB cfg "password"
  username <- case command & auth & Options.username of
          Nothing  -> readOption cfg "username"
          (Just x) -> return $ T.pack x
  currentSid <- readSid username
  apiRoot <- getApiEndpointRootUrl cfg
  let creds = ClientCredentials {
                password = password,
                appkey  = "5ab793910e36584cd81622e5eb77d3d1",
                sid     = Right currentSid,
                username    = username,  
                secret  = "8543db8deccb4b0fcb753291c53f8f4f",
                endpoint = apiRoot
                } & updateCreds $ command & auth
  actualSid <- case currentSid of
    ("") -> authRequest creds
    _    -> return creds
  case actualSid & sid of
    (Right x)  -> return creds
    (Left err) -> askPassword username err >>= \s -> case s of 
        (Right pass) -> authRequest $ creds {password = pass}
        (Left msg)   -> return creds {sid = Left msg}


main :: IO ()
main = do
  command <- customExecParser parserPrefs mainParser 
  parseOpt command >>= (`printResult` " ")  where

      parseOpt :: Args -> IO (Either BL.ByteString [BL.ByteString])
      parseOpt x | x & versionFlag = return $ Right 
                                              ["hapidry", 
                                               BL.pack $ showVersion P.version,
                                               BL.concat ["(", $(gitHash), ")"]]
                 | otherwise       = getCreds x >>= parseOpt' (x & commands) where

        parseOpt' :: Commands -> ClientCredentials -> IO (Either BL.ByteString [BL.ByteString])
        parseOpt' p@None client = callProcess "hapidry" ["--help"] >> return (Right [])
        parseOpt' _ ClientCredentials{sid=(Left x)} = return $ Left x
        parseOpt' p@Post {} client = createPost p client >>= \x -> return $ getResponseField "postid" x
        parseOpt' s@Send {} client = sendUmail s client >>= \x -> return $ getResponseField "message" x
        parseOpt' c@Comment {} client = createComment c client >>= \x -> return $ getResponseField "message" x
        parseOpt' n@Notify {} client = getNotifications n client >> return (Right [""])
        parseOpt' p@Blog {} client = readPost p client >> return (Right [""])
        parseOpt' p@Umail {} client = readUmail p client >> return (Right [""])
