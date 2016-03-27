{-# LANGUAGE  OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}


import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)


import qualified Data.Configurator as C
import Control.Lens ((&))
import Api

import Options
import Utils


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
  parseOpt (command & commands) client >> mempty where
      parseOpt p@Post {} client = createPost p client >> mempty
      parseOpt s@Send {} client = sendUmail s client >> mempty
      parseOpt n@Notify {} client = getNotifications n client

