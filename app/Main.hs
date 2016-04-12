{-# LANGUAGE  OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}


import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)


import qualified Data.Configurator as C
import Control.Lens ((&))
import Api
import Internal.Api

import Options
import Utils


import Data.Aeson

import Prelude hiding (putStr)
import Data.ByteString.Char8 (putStr, pack)
import qualified Data.Text.Encoding as E
import Data.ByteString.UTF8 (fromString)


import qualified Data.Text    as T
import qualified Data.Text.IO as T
-- main :: IO ()
-- main = putStr $ fromString "čušpajž日本語"

main :: IO ()
main = do
  command <- execParser mainParser 
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
  parseOpt (command & commands) client >>= (\x -> putStr $ (fromString $ show x) + 1)  where
      -- parseOpt p@Post {} client = createPost p client -- >> mempty
      -- parseOpt s@Send {} client = sendUmail s client -- >> mempty
      -- parseOpt c@Comment {} client = createComment c client -- >> mempty
      parseOpt _ client = postsFromJson <$> apiPost client [("method", "post.get"),
                                                            ("type", "favorites")]

      --parseOpt n@Notify {} client = getNotifications n client >> return (Right ["Ok"])
   
