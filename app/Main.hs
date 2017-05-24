{-# LANGUAGE  OverloadedStrings, TemplateHaskell, DuplicateRecordFields, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import qualified Data.Configurator as C
import Control.Lens ((&))
import Api
import Internal.Api (authRequest)

import Options
import Utils


import Prelude hiding (putStr)
import Data.ByteString.Char8 (putStr, pack)


import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString, pack, unpack, putStr, putStrLn, concat)


import Data.Version (showVersion)
import Development.GitRev (gitHash)
import qualified Paths_hapidry as P (version)

import System.Process (callProcess)
import System.Exit
import Control.Exception
import qualified System.Console.Haskeline as HL (runInputT, defaultSettings, getPassword)


type Delimeter = T.Text

errorMessage :: T.Text -> T.Text
errorMessage m = T.concat ["Конфигурационный файл не существует! Создайте ",
                           m,
                           " вручную или используйте hapidry-generate"]

--askPassword :: T.Text -> BL.ByteString -> Either BL.ByteString Text
-- askPassword username = HL.runInputT HL.defaultSettings $ do
--   p <- getPassword (Just '') 
--                    ("Enter password for " ++ (T.unpack username) ++ ":")

printResult :: Either BL.ByteString [BL.ByteString] -> Delimeter -> IO ()
printResult (Left x)  _ = printError x
printResult (Right xs) d = mapM_ (\x -> BL.putStr x >> T.putStr d) xs >> putStrLn ""

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
  let creds = ClientCredentials {
                password = password,
                appkey  = "5ab793910e36584cd81622e5eb77d3d1",
                sid     = Right currentSid,
                username    = username,  
                secret  = "8543db8deccb4b0fcb753291c53f8f4f"
                } & updateCreds $ command & auth
  actualSid <- case currentSid of
    ("") -> authRequest creds
    _    -> return creds
  case actualSid & sid of
    (Right x)  -> return creds
    (Left err) -> creds {sid = askPassword username err}

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
