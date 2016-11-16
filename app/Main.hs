{-# LANGUAGE  OverloadedStrings, TemplateHaskell, DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import qualified Data.Configurator as C
import Control.Lens ((&))
import Api
import Internal.Api (authRequest)
import qualified Internal.Json as IJ--del
import qualified Json as J

import Options
import Utils

import Data.Maybe

import Data.Aeson

import Prelude hiding (putStr)
import Data.ByteString.Char8 (putStr, pack)
import qualified Data.Text.Encoding as E
-- import Data.ByteString.UTF8 (fromString)


import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString, pack, unpack, putStr, concat)


import Data.Version (showVersion)
import Development.GitRev (gitHash)
import qualified Paths_hapidry as P (version)
-- import System.Directory
-- import System.FilePath


type Delimeter = T.Text
extractP (IJ.PostList x) = x
extractU (IJ.MessageList x) = x
fromRight (Right x) = x



printResult :: Either Integer [BL.ByteString] -> Delimeter -> IO ()
printResult (Left x)  _ = error $ show x
printResult (Right xs) d = mapM_ (\x -> BL.putStr x >> T.putStr d) xs >> putStrLn ""

getCreds :: Args -> IO ClientCredentials
getCreds command = do
  cfg <- C.load [C.Required (command & config)]
  password <- readOptionB cfg "password"
  username <- case command & auth & Options.username of
          Nothing  -> readOption cfg "username"
          (Just x) -> return $ T.pack x
  -- let path = T.unpack $ T.concat ["/tmp/hapidry_", username]
  -- exist <- doesFileExist path
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

        parseOpt' p@Post {} client = createPost p client -- >> mempty
        parseOpt' s@Send {} client = sendUmail s client -- >> mempty
        parseOpt' c@Comment {} client = createComment c client -- >> mempty
        parseOpt' n@Notify {} client = getNotifications n client >> return (Right [""])
        parseOpt' p@Blog {} client = readPost p client >> return (Right [""])
        parseOpt' p@Umail {} client = readUmail p client >> return (Right [""])

  -- parseOpt (command & commands) client >>= print where
  -- parseOpt (command & commands) client >>= (\x -> BL.putStr $ fromRight x) where
  -- parseOpt (command & commands) client >>= (T.putStr . IJ.messageHtml . head)  where
  --     parseOpt p@Post {} client = createPost p client -- >> mempty
  --     parseOpt s@Send {} client = sendUmail s client -- >> mempty
  --     parseOpt c@Comment {} client = createComment c client -- >> mempty
  --     parseOpt post client =  extractP <$> fromJust <$> postsFromJson <$> apiPost client [("method", "post.get"),
  --                                                           ("type", "favorites")]
      -- parseOpt _ client =  extractU <$> fromJust <$> umailsFromJson <$> (umailGet client [])
      -- parseOpt _ client = (umailGet client [])

      --parseOpt n@Notify {} client = getNotifications n client >> return (Right ["Ok"])
   
