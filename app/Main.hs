{-# LANGUAGE  OverloadedStrings, TemplateHaskell, DuplicateRecordFields #-}
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

type Delimeter = T.Text

printResult :: Either BL.ByteString [BL.ByteString] -> Delimeter -> IO ()
printResult (Left x)  _ = printError x
printResult (Right xs) d = mapM_ (\x -> BL.putStr x >> T.putStr d) xs >> putStrLn ""

getCreds :: Args -> IO ClientCredentials
getCreds command = do
  cfg <- C.load [C.Required (command & config)]
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
  case currentSid of
    ("") -> authRequest creds
    _    -> return creds



main :: IO ()
main = do
  command <- customExecParser parserPrefs mainParser 
  parseOpt command >>= (`printResult` " ")  where
      parseOpt x | x & versionFlag = return $ Right 
                                              ["hapidry", 
                                               BL.pack $ showVersion P.version,
                                               BL.concat ["(", $(gitHash), ")"]]
                 | otherwise       = getCreds x >>= parseOpt' (x & commands) where

        parseOpt' p@Post {} client = (createPost p client) >>= \x -> return $ getResponseField "postid" x
        parseOpt' s@Send {} client = sendUmail s client >>= \x -> return $ getResponseField "message" x
        parseOpt' c@Comment {} client = createComment c client >>= \x -> return $ getResponseField "message" x
        parseOpt' n@Notify {} client = getNotifications n client >> return (Right [""])
        parseOpt' p@Blog {} client = readPost p client >> return (Right [""])
        parseOpt' p@Umail {} client = readUmail p client >> return (Right [""])
        parseOpt' p@None client = callProcess "hapidry" ["--help"] >> return (Right [])